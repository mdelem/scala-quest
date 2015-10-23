package com.github.mdelem.scalaquest

trait Item {
  def name: Option[String]
}

case class SimpleItem(name: Option[String] = None, proposition: String, acceptedType: String = "String") extends Item

case class ComplexItem(name: Option[String] = None, items: Seq[SimpleItem], randomized: Boolean = false) extends Item

case class ItemGroup(name: Option[String] = None, items: Seq[Item], randomized: Boolean = false)

case class Part(name: Option[String] = None, groups: Seq[ItemGroup], randomized: Boolean = false)

case class Questionnaire(name: Option[String] = None, parts: Seq[Part], randomized: Boolean = false)

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
  def apply(items: Seq[SimpleItem]): ComplexItem = ComplexItem(None, items)
  def apply(items: Seq[SimpleItem], randomized: Boolean): ComplexItem = ComplexItem(None, items, randomized)
}

object SimpleItem {
  def apply(proposition: String): SimpleItem = SimpleItem(None, proposition)
  def apply(proposition: String, acceptedType: String): SimpleItem = SimpleItem(None, proposition, acceptedType)
}

object Implicits {
  implicit def stringToSomeString(s: String) = Some(s)

  implicit def itemToItemGroup(i:Item) = ItemGroup(Seq(i))
  
  implicit def partAsSeq(p : Part) = Seq(p)
  implicit class PartAsSeq(p : Part) {
    def ~(next : Part) : Seq[Part] = Seq(p, next)
  }
  implicit class PartSeq(p : Seq[Part]) {
    def ~(next : Part) : Seq[Part] = p ++ next
  }
  
  implicit def itemGroupAsSeq(p : ItemGroup) = Seq(p)
  implicit class ItemGroupAsSeq(p : ItemGroup) {
    def ~(next : ItemGroup) : Seq[ItemGroup] = Seq(p, next)
  }
  implicit class ItemGroupSeq(p : Seq[ItemGroup]) {
    def ~(next : ItemGroup) : Seq[ItemGroup] = p ++ next
  }
  
  implicit def itemAsSeq(i : Item) = Seq(i)
  implicit class ItemAsSeq(i : Item) {
    def ~(next : Item) : Seq[Item] = Seq(i, next)
  }
  implicit class ItemSeq(i : Seq[Item]) {
    def ~(next : Item) : Seq[Item] = i ++ next
  }
  
  implicit def simpleItemAsSeq(i : SimpleItem) = Seq(i)
  implicit class SimpleItemAsSeq(i : SimpleItem) {
    def ~(next : SimpleItem) : Seq[SimpleItem] = Seq(i, next)
  }
  implicit class SimpleItemSeq(i : Seq[SimpleItem]) {
    def ~(next : SimpleItem) : Seq[SimpleItem] = i ++ next
  }
}