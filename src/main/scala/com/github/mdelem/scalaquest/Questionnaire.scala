package com.github.mdelem.scalaquest

trait Item {
  def name: Option[String]
}

trait QuestionnaireNode

case class SimpleItem[T](name: Option[String] = None, proposition: String) extends Item

case class ComplexItem(name: Option[String] = None, items: Seq[SimpleItem[_]], randomized: Boolean = false) extends Item

case class ItemGroup(name: Option[String] = None, items: Seq[Item], randomized: Boolean = false) extends QuestionnaireNode

case class Part(name: Option[String] = None, groups: Seq[ItemGroup], randomized: Boolean = false) extends QuestionnaireNode

case class Questionnaire(name: Option[String] = None, parts: Seq[Part], randomized: Boolean = false) extends QuestionnaireNode {
  val parent: Map[QuestionnaireNode, QuestionnaireNode] = (parts.flatMap { p =>
    p.groups.map(g => (g, p)) :+ (p, this)
  }).toMap

}

object Questionnaire {
  def apply(parts: Seq[Part]): Questionnaire = Questionnaire(None, parts)
  def apply(parts: Seq[Part], randomized: Boolean): Questionnaire = Questionnaire(None, parts, randomized)
}

object Part {
  def apply(groups: Seq[ItemGroup]): Part = Part(None, groups)
  def apply(groups: Seq[ItemGroup], randomized: Boolean): Part = Part(None, groups, randomized)
}

object ItemGroup {
  def apply(items: Seq[Item]): ItemGroup = ItemGroup(None, items)
  def apply(items: Seq[Item], randomized: Boolean): ItemGroup = ItemGroup(None, items, randomized)
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
  implicit def itemToItemGroup(i: Item) = ItemGroup(Seq(i))
  implicit def itemToSeqItemGroup(i: Item) = Seq(ItemGroup(Seq(i)))

  implicit class ItemAsSeq(i: Item) {
    def ~(next: Item): Seq[Item] = Seq(i, next)
  }
  implicit class ItemSeq(i: Seq[Item]) {
    def ~(next: Item): Seq[Item] = i :+ next
    def ~(next: ItemGroup): Seq[ItemGroup] = i.map(j => ItemGroup(Seq(j))) :+ next
  }

  implicit class SimpleItemAsSeq[T](i: SimpleItem[T]) {
    def ~[U](next: SimpleItem[U]): Seq[SimpleItem[_]] = Seq(i, next)
    def ~(next: ItemGroup): Seq[ItemGroup] = Seq(ItemGroup(Seq(i)), next)
    def ~(next: ComplexItem): Seq[Item] = Seq(i, next)
  }
  implicit class SimpleItemSeq(i: Seq[SimpleItem[_]]) {
    def ~[U](next: SimpleItem[U]): Seq[SimpleItem[_]] = i :+ next
    def ~(next: ItemGroup): Seq[ItemGroup] = i.map(j => ItemGroup(Seq(j))) :+ next
    def ~(next: ComplexItem): Seq[Item] = i :+ next
  }

  implicit class ComplexItemAsSeq(i: ComplexItem) {
    def ~(next: ItemGroup): Seq[ItemGroup] = Seq(ItemGroup(Seq(i)), next)
    def ~(next: Item): Seq[Item] = Seq(i, next)
  }
}