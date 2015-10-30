package com.github.mdelem.scalaquest

trait Item {
  def name: Option[String]
}

trait QuestionnaireNode

case class SimpleItem[T](name: Option[String] = None, proposition: String) extends Item

case class ComplexItem(name: Option[String] = None, items: Seq[SimpleItem[_]], randomized: Boolean = false) extends Item {
  val item: Map[String, SimpleItem[_]] = items.filter(_.name.isDefined).map(i => (i.name.get, i)).toMap
  //  def item[T]: Map[String, SimpleItem[T]] = itemMap.asInstanceOf[Map[String, SimpleItem[T]]]
}

case class ItemGroup(name: Option[String] = None, randomized: Boolean = false, items: Seq[Item]) extends QuestionnaireNode {
  private val itemMap: Map[String, Item] = items.filter(_.name.isDefined).map(i => (i.name.get, i)).toMap
  private val simpleItemMap: Map[String, SimpleItem[_]] = items.filter(i => i.name.isDefined && i.isInstanceOf[SimpleItem[_]]).map(i => (i.name.get, i.asInstanceOf[SimpleItem[_]])).toMap
  def simpleItem: Map[String, SimpleItem[_]] =
    (simpleItemMap ++ complexItem.values.flatMap(ci => ci.item))
  val complexItem: Map[String, ComplexItem] = items.filter(i => i.name.isDefined && i.isInstanceOf[ComplexItem]).map(i => (i.name.get, i.asInstanceOf[ComplexItem])).toMap
  val item = simpleItemMap ++ complexItem ++ complexItem.values.flatMap(ci => ci.item)
}

case class Part(name: Option[String] = None, randomized: Boolean = false, groups: Seq[ItemGroup]) extends QuestionnaireNode {
  val group: Map[String, ItemGroup] = groups.filter(_.name.isDefined).map(g => (g.name.get, g)).toMap
  def complexItem: Map[String, ComplexItem] = groups.flatMap(_.complexItem).toMap
  def simpleItem: Map[String, SimpleItem[_]] = groups.flatMap(_.simpleItem).toMap
  def item: Map[String, Item] = groups.flatMap(_.item).toMap
}

case class Questionnaire(name: Option[String] = None, randomized: Boolean = false, parts: Seq[Part]) extends QuestionnaireNode {
  val part: Map[String, Part] = parts.filter(_.name.isDefined).map(p => (p.name.get, p)).toMap
  val group: Map[String, ItemGroup] = parts.flatMap(_.group).toMap
  def complexItem: Map[String, ComplexItem] = parts.flatMap(_.complexItem).toMap
  def simpleItem: Map[String, SimpleItem[_]] = parts.flatMap(_.simpleItem).toMap
  def item: Map[String, Item] = parts.flatMap(_.item).toMap

  /**
   * A map listing the parents of the questionnaire nodes of this questionnaire
   */
  val parent: Map[QuestionnaireNode, QuestionnaireNode] = (parts.flatMap { p =>
    p.groups.map(g => (g, p)) :+ (p, this)
  }).toMap
}

object Questionnaire {
  def apply(parts: Seq[Part]): Questionnaire = Questionnaire(None, false, parts)
  def apply(parts: Seq[Part], randomized: Boolean): Questionnaire = Questionnaire(None, randomized, parts)
  def apply(name: Option[String], parts: Seq[Part]): Questionnaire = Questionnaire(name, false, parts)

}

object Part {
  def apply(groups: Seq[ItemGroup]): Part = Part(None, groups)
  def apply(groups: Seq[ItemGroup], randomized: Boolean): Part = Part(None, randomized, groups)
  def apply(name: Option[String], groups: Seq[ItemGroup]): Part = Part(name, false, groups)
}

object ItemGroup {
  def apply(items: Seq[Item]): ItemGroup = ItemGroup(None, false, items)
  def apply(items: Seq[Item], randomized: Boolean): ItemGroup = ItemGroup(None, randomized, items)
  def apply(name: Option[String], items: Seq[Item]): ItemGroup = ItemGroup(name, false, items)
}

object ComplexItem {
  def apply(items: Seq[SimpleItem[_]]): ComplexItem = ComplexItem(None, items)
  def apply(items: Seq[SimpleItem[_]], randomized: Boolean): ComplexItem = ComplexItem(None, items, randomized)
}

object SimpleItem {
  def apply[T](proposition: String): SimpleItem[T] = SimpleItem(None, proposition)
}

object Implicits {
  implicit def stringToSomeString(s: String) = Some(s)

  implicit def partAsSeq(p: Part) = Seq(p)
  implicit class PartAsSeq(p: Part) {
    def ~(next: Part): Seq[Part] = Seq(p, next)
  }
  implicit class PartSeq(p: Seq[Part]) {
    def ~(next: Part): Seq[Part] = p :+ next
  }

  implicit def itemGroupAsSeq(p: ItemGroup) = Seq(p)
  implicit class ItemGroupAsSeq(p: ItemGroup) {
    def ~(next: ItemGroup): Seq[ItemGroup] = Seq(p, next)
  }
  implicit class ItemGroupSeq(p: Seq[ItemGroup]) {
    def ~(next: ItemGroup): Seq[ItemGroup] = p :+ next
  }

  implicit def itemAsSeq(i: Item) = Seq(i)
  implicit def simpleItemAsSeq[T](i: SimpleItem[T]) = Seq(i)
  implicit def itemToItemGroup(i: Item) = ItemGroup(i.name, Seq(i))
  implicit def itemToSeqItemGroup(i: Item) = Seq(ItemGroup(i.name, Seq(i)))

  implicit class ItemAsSeq(i: Item) {
    def ~(next: Item): Seq[Item] = Seq(i, next)
  }
  implicit class ItemSeq(i: Seq[Item]) {
    def ~(next: Item): Seq[Item] = i :+ next
    def ~(next: ItemGroup): Seq[ItemGroup] = i.map(j => ItemGroup(j.name, Seq(j))) :+ next
  }

  implicit class SimpleItemAsSeq[T](i: SimpleItem[T]) {
    def ~[U](next: SimpleItem[U]): Seq[SimpleItem[_]] = Seq(i, next)
    def ~(next: ItemGroup): Seq[ItemGroup] = Seq(ItemGroup(i.name, Seq(i)), next)
    def ~(next: ComplexItem): Seq[Item] = Seq(i, next)
  }
  implicit class SimpleItemSeq(i: Seq[SimpleItem[_]]) {
    def ~[U](next: SimpleItem[U]): Seq[SimpleItem[_]] = i :+ next
    def ~(next: ItemGroup): Seq[ItemGroup] = i.map(j => ItemGroup(j.name, Seq(j))) :+ next
    def ~(next: ComplexItem): Seq[Item] = i :+ next
  }

  implicit class ComplexItemAsSeq(i: ComplexItem) {
    def ~(next: ItemGroup): Seq[ItemGroup] = Seq(ItemGroup(i.name, Seq(i)), next)
    def ~(next: Item): Seq[Item] = Seq(i, next)
  }
}