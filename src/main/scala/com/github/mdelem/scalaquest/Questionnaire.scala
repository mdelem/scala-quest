package com.github.mdelem.scalaquest

trait Item {
  def name: String
}

trait QuestionnaireNode

case class SimpleItem[+T](name: String, proposition: String, acceptedValues: Seq[T] = Seq()) extends Item

case class ComplexItem(name: String, randomized: Boolean = false, items: Seq[SimpleItem[_]]) extends Item {
  val item: Map[String, SimpleItem[_]] = items.map(i => (i.name, i)).toMap
}

case class ItemGroup(name: String, randomized: Boolean = false, items: Seq[Item]) extends QuestionnaireNode {
  private val itemMap: Map[String, Item] = items.map(i => (i.name, i)).toMap
  private val simpleItemMap: Map[String, SimpleItem[_]] = items.flatMap {
    case i @ SimpleItem(name, _, _) => Some((name, i))
    case _                          => None
  }.toMap
  def simpleItem: Map[String, SimpleItem[_]] =
    (simpleItemMap ++ complexItem.values.flatMap(ci => ci.item))
  val complexItem: Map[String, ComplexItem] = items.flatMap {
    case i @ ComplexItem(name, _, _) => Some((name, i))
    case _                           => None
  }.toMap
  val item = simpleItemMap ++ complexItem ++ complexItem.values.flatMap(ci => ci.item)
}

case class Part(name: String, randomized: Boolean = false, groups: Seq[ItemGroup]) extends QuestionnaireNode {
  val group: Map[String, ItemGroup] = groups.map(g => (g.name, g)).toMap
  def complexItem: Map[String, ComplexItem] = groups.flatMap(_.complexItem).toMap
  def simpleItem: Map[String, SimpleItem[_]] = groups.flatMap(_.simpleItem).toMap
  def item: Map[String, Item] = groups.flatMap(_.item).toMap
}

case class Questionnaire(name: String, randomized: Boolean = false, parts: Seq[Part]) extends QuestionnaireNode {
  val part: Map[String, Part] = parts.map(p => (p.name, p)).toMap
  val group: Map[String, ItemGroup] = parts.flatMap(_.group).toMap
  def complexItem: Map[String, ComplexItem] = parts.flatMap(_.complexItem).toMap
  def simpleItem: Map[String, SimpleItem[_]] = parts.flatMap(_.simpleItem).toMap
  def item: Map[String, Item] = parts.flatMap(_.item).toMap

  private val itemGroupParentMap: Map[ItemGroup, Part] = (parts.flatMap { p =>
    p.groups.map(g => (g, p))
  }).toMap

  def parent(n: ItemGroup): Part = {
    itemGroupParentMap(n)
  }

  def parent(n: Part): Questionnaire = {
    this
  }

}

object Questionnaire {
  def apply(name: String, parts: Seq[Part]): Questionnaire = Questionnaire(name, false, parts)

}

object Part {
  def apply(name: String, groups: Seq[ItemGroup]): Part = Part(name, false, groups)
  def apply(name: String, randomized: Boolean, items: => Seq[Item] /*Anti type erasure magic*/): Part = Part(name, false, items.map(i => ItemGroup(i.name, Seq(i))))
  def apply(name: String, items: => Seq[Item]): Part = Part(name, false, items.map(i => ItemGroup(i.name, Seq(i))))
}

object ItemGroup {
  def apply(name: String, items: Seq[Item]): ItemGroup = ItemGroup(name, false, items)
}

object ComplexItem {
  def apply(name: String, items: Seq[SimpleItem[_]]): ComplexItem = ComplexItem(name, false, items)

}

object Implicits {

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
    def ~(next: Item): Seq[ItemGroup] = Seq(p, ItemGroup(next.name, Seq(next)))
  }
  implicit class ItemGroupSeq(p: Seq[ItemGroup]) {
    def ~(next: ItemGroup): Seq[ItemGroup] = p :+ next
    def ~(next: Item): Seq[ItemGroup] = p :+ ItemGroup(next.name, Seq(next))
  }

  implicit def itemAsSeq(i: Item) = Seq(i)
  implicit def simpleItemAsSeq[T](i: SimpleItem[T]) = Seq(i)
  //  implicit def itemToItemGroup(i: Item) = ItemGroup(i.name, Seq(i))
  //  implicit def itemToSeqItemGroup(i: Item) = Seq(ItemGroup(i.name, Seq(i)))

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