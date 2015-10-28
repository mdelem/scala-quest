package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._

class QuestionnaireSpec extends FlatSpec with Matchers {

  val q = Questionnaire(
    name = "q1",
    Part(
        name = "p1",
      ItemGroup(name = "ig1",
        SimpleItem[Int]("i1", "Age") ~
          SimpleItem[String]("i2", "Gender")) ~
        SimpleItem[Int]("i3", "I like chocolate") ~
        SimpleItem[Int]("i4","I like vanilla") ~
        ComplexItem("ci1",
          SimpleItem[Boolean]("i5","I prefer chocolate") ~
            SimpleItem[Boolean]("i6","I prefer vanilla"))))

  "A Questionnaire" should "contains parts" in {
    q.parts.size should be(1)
  }
  "A Part" should "contains item groups" in {
    q.part("p1").groups.size should be(4)
  }
  "An Item Group" should "contains items" in {
    q.part("p1").group("ig1").items.size should be(2)
  }
  "An Item" should "is either a simple item or a complex item" in {
    q.part("p1").simpleItem("i3") shouldBe an[SimpleItem[_]]
    q.part("p1").item("ci1") shouldBe an[ComplexItem]
  }
  "A Complex Item" should "contains simple items" in {
    q.part("p1").complexItem("ci1").item("i5") shouldBe an[SimpleItem[_]]
  }
  "A Simple Item" should "contains a proposition and specifies a response type" in {
    val i = q.part("p1").simpleItem("i3").proposition should be("I like chocolate")
  }

  // Building a complex questionnaire to check if the implicits 
  // work as expected and there are no compile time errors
  Questionnaire(
    name = "q2",
    Part(
      SimpleItem[Int]("I like chocolate") ~
        SimpleItem[Int]("I like vanilla") ~
        ComplexItem(
          SimpleItem[Boolean]("I prefer chocolate") ~
            SimpleItem[Boolean]("I prefer vanilla"))) ~
      Part(
        ComplexItem(
          SimpleItem[Boolean]("I prefer chocolate") ~
            SimpleItem[Boolean]("I prefer vanilla")) ~
          SimpleItem[Int]("I like chocolate") ~
          SimpleItem[Int]("I like vanilla") ~
          ItemGroup(
            SimpleItem[Int]("d1", "Age") ~
              ComplexItem(
                SimpleItem[Boolean]("I prefer vanilla"))) ~
            ItemGroup(
              ComplexItem(
                SimpleItem[Boolean]("I prefer vanilla")) ~
                SimpleItem[Int]("d1", "Age"))) ~
        Part(
          SimpleItem[Int]("I like chocolate")) ~
          Part(
            ItemGroup(
              ComplexItem(
                SimpleItem[Boolean]("I prefer vanilla")))) ~
            Part(
              SimpleItem[Int]("I like chocolate") ~
                ItemGroup(
                  SimpleItem[Int]("d1", "Age"))))

}