package com.github.mdelem.scalaquest

import org.scalatest._
import javax.xml.bind.ValidationException
import Validator.{ ValidatesInt, ValidatesString }

class RequestValidatorSpec extends FlatSpec with Matchers {

  val q = Questionnaire(
    name = "q1",
    Seq(Part(
      name = "p1",
      Seq(ItemGroup(name = "ig1",
        Seq(SimpleItem[Int]("i1", "Age")(ValidatesInt(0, 140, 1)),
          SimpleItem[String]("i2", "Gender")(ValidatesString(Set("M", "F", "O"))))),
        ItemGroup(name = "ig3", Seq(SimpleItem[Int]("i3", "I like chocolate"))),
        ItemGroup(name = "ig4", Seq(SimpleItem[Int]("i4", "I like vanilla"))),
        ItemGroup(name = "ig5", Seq(ComplexItem("ci1",
          Seq(SimpleItem[Boolean]("i5", "I prefer chocolate"),
            SimpleItem[Boolean]("i6", "I prefer vanilla"))))))),
      Part(
        name = "p2",
        Seq(ItemGroup(name = "ig6", Seq(SimpleItem[Boolean]("i7", "I answered this questionnaire truthfully")))))))

  //TODO: replace cast with something more pretty
  val item = q.simpleItem("i3").asInstanceOf[SimpleItem[Int]]
  val answer = Answer(item, "10")
  val r = Request(q.group("ig3"), answer)
  "Getting the answer for an item'" should "return its value" in {
    r.answer[Int](item).get shouldBe (10)
  }

  //TODO: other validation tests

}