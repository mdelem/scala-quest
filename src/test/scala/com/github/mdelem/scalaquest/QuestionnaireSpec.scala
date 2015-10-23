package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._

class QuestionnaireSpec extends FlatSpec with Matchers {
  val q = Questionnaire(
    name = "q1",
    Seq(Part(
      Seq(
        ItemGroup(
          Seq(
            SimpleItem("Age", "Integer"),
            SimpleItem("Gender"))),
        SimpleItem("I like chocolate", "1-10 Scale"),
        SimpleItem("I like vanilla", "1-10 Scale"),
        ComplexItem(
          Seq(
            SimpleItem("I prefer chocolate", "Boolean"),
            SimpleItem("I prefer vanilla", "Boolean")))))))

  "A Questionnaire" should "contains parts" in {
    q.parts.size should be(1)
  }
  "A Part" should "contains item groups" in {
    q.parts.head.groups.size should be(4)
  }
  "An Item Group" should "contains items" in {
    q.parts.head.groups.head.items.size should be(2)
  }
  "An Item" should "is either a simple item or a complex item" in {
    q.parts.head.groups.drop(1).head.items.head shouldBe an[SimpleItem]
    q.parts.head.groups.drop(3).head.items.head shouldBe an[ComplexItem]
  }
  "A Complex Item" should "contains simple items" in {
    q.parts.head.groups.drop(3).head.items.head.asInstanceOf[ComplexItem].items.head shouldBe an[SimpleItem]
  }
  "A Simple Item" should "contains a proposition and specifies a response type" in {
    val i = q.parts.head.groups.drop(1).head.items.head
    i.asInstanceOf[SimpleItem].proposition should be("I like chocolate")
    i.asInstanceOf[SimpleItem].acceptedType should be("1-10 Scale")
  }
}