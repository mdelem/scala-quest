package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.engine.SimpleItem
import com.github.mdelem.scalaquest.engine.Questionnaire
import com.github.mdelem.scalaquest.engine.Part
import com.github.mdelem.scalaquest.engine.ItemGroup
import com.github.mdelem.scalaquest.engine.ComplexItem

class QuestionnaireSpec extends FlatSpec with Matchers {

   val q = Questionnaire(
    name = "q1",
    Seq(Part(
      name = "p1",
      Seq(ItemGroup(name = "ig1",
        Seq(SimpleItem[Int]("i1", "Age"),
          SimpleItem[String]("i2", "Gender"))),
        ItemGroup(name = "ig3", Seq(SimpleItem[Int]("i3", "I like chocolate"))),
        ItemGroup(name = "ig4", Seq(SimpleItem[Int]("i4", "I like vanilla"))),
        ItemGroup(name = "ig5", Seq(ComplexItem("ci1",
          Seq(SimpleItem[Boolean]("i5", "I prefer chocolate"),
            SimpleItem[Boolean]("i6", "I prefer vanilla"))))))),
      Part(
        name = "p2",
        Seq(ItemGroup(name = "ig6", Seq(SimpleItem[Boolean]("i7", "I answered this questionnaire truthfully")))))))

  "A Questionnaire" should "contains parts" in {
    q.parts.size should be(2)
  }
  "A Part" should "contains item groups" in {
    q.part("p1").groups.size should be(4)
  }
  "An Item Group" should "contains items" in {
    q.group("ig1").items.size should be(2)
  }
  "An Item" should "is either a simple item or a complex item" in {
    q.part("p1").simpleItem("i3") shouldBe an[SimpleItem[_]]
    q.item("ci1") shouldBe an[ComplexItem]
  }
  "A Complex Item" should "contains simple items" in {
    q.part("p1").item("i5") shouldBe an[SimpleItem[_]]
  }
  "A Simple Item" should "contains a proposition and specifies a response type" in {
    val i = q.part("p1").simpleItem("i3").proposition should be("I like chocolate")
  }

}