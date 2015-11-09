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

  // Building a complex questionnaire to check if the implicits 
  // work as expected and there are no compile time errors
  Questionnaire(
    name = "q2",
    Part("p1", 
      SimpleItem[Int]("i1", "I like chocolate") ~
        SimpleItem[Int]("i2", "I like vanilla") ~
        ComplexItem("i3", 
          SimpleItem[Boolean]("i4", "I prefer chocolate") ~
            SimpleItem[Boolean]("i5", "I prefer vanilla"))) ~
      Part("p2", 
        ComplexItem("i6", 
          SimpleItem[Boolean]("i7", "I prefer chocolate") ~
            SimpleItem[Boolean]("i8", "I prefer vanilla")) ~
          SimpleItem[Int]("i9", "I like chocolate") ~
          SimpleItem[Int]("i10", "I like vanilla") ~
          ItemGroup("i11", 
            SimpleItem[Int]("d1", "Age") ~
              ComplexItem("i12", 
                SimpleItem[Boolean]("i13", "I prefer vanilla"))) ~
            ItemGroup("i14", 
              ComplexItem("i15", 
                SimpleItem[Boolean]("i16", "I prefer vanilla")) ~
                SimpleItem[Int]("d2", "Age"))) ~
        Part(name = "p3", 
          SimpleItem[Int]("i17", "I like chocolate")) ~
          Part("p4", 
            ItemGroup("i18", 
              ComplexItem("i19", 
                SimpleItem[Boolean]("i20", "I prefer vanilla")))) ~
            Part("p5", 
              SimpleItem[Int]("i20", "I like chocolate") ~
                ItemGroup("i21", 
                  SimpleItem[Int]("d3", "Age"))))

}