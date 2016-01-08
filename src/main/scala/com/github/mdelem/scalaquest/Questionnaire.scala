package com.github.mdelem.scalaquest

import Validator.Validates

trait Item {
  def name: String
}

trait QuestionnaireNode

case class SimpleItem[T](name: String, proposition: String, reversed: Boolean = false)(implicit val validator: Validates[T]) extends Item

case class ComplexItem(name: String, proposition: Option[String], randomized: Boolean = false, items: Seq[SimpleItem[_]]) extends Item {
  val item: Map[String, SimpleItem[_]] = items.map(i => (i.name, i)).toMap
}

case class ItemGroup(name: String, randomized: Boolean = false, items: Seq[Item]) extends QuestionnaireNode {
  private val itemMap: Map[String, Item] = items.map(i => (i.name, i)).toMap
  private val simpleItemMap: Map[String, SimpleItem[_]] = items.flatMap {
    case i @ SimpleItem(name, _, _) => Some((name, i))
    case _                          => None
  }.toMap
  val complexItem: Map[String, ComplexItem] = items.flatMap {
    case i @ ComplexItem(name, _,_, _) => Some((name, i))
    case _                           => None
  }.toMap
  val simpleItem: Map[String, SimpleItem[_]] =
    (simpleItemMap ++ complexItem.values.flatMap(ci => ci.item))
  val item = simpleItemMap ++ complexItem ++ complexItem.values.flatMap(ci => ci.item)
}

case class Part(name: String, randomized: Boolean = false, groups: Seq[ItemGroup]) extends QuestionnaireNode {
  val group: Map[String, ItemGroup] = groups.map(g => (g.name, g)).toMap
  val complexItem: Map[String, ComplexItem] = groups.flatMap(_.complexItem).toMap
  val simpleItem: Map[String, SimpleItem[_]] = groups.flatMap(_.simpleItem).toMap
  val item: Map[String, Item] = groups.flatMap(_.item).toMap
}

case class Questionnaire(name: String, randomized: Boolean = false, parts: Seq[Part]) extends QuestionnaireNode {
  val part: Map[String, Part] = parts.map(p => (p.name, p)).toMap
  val group: Map[String, ItemGroup] = parts.flatMap(_.group).toMap
  val complexItem: Map[String, ComplexItem] = parts.flatMap(_.complexItem).toMap
  val simpleItem: Map[String, SimpleItem[_]] = parts.flatMap(_.simpleItem).toMap
  val item: Map[String, Item] = parts.flatMap(_.item).toMap

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
}

object ItemGroup {
  def apply(name: String, items: Seq[Item]): ItemGroup = ItemGroup(name, false, items)
}

object ComplexItem {
  def apply(name: String, items: Seq[SimpleItem[_]]): ComplexItem = ComplexItem(name, None, false, items)

}
