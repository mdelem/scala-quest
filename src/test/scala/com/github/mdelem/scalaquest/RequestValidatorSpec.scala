package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._

class RequestValidatorSpec extends FlatSpec with Matchers {

  @WithAccessors
  val q = Questionnaire(
    "q1",
    Part(
      name = "p1",
      ItemGroup(name = "ig1",
        SimpleItem[Int]("i1", "Age") ~
          SimpleItem[String]("i2", "Gender")) ~
        SimpleItem[Int]("i3", "I like chocolate") ~
        SimpleItem[Int]("i4", "I like vanilla") ~
        ComplexItem("ci1",
          SimpleItem[Boolean]("i5", "I prefer chocolate") ~
            SimpleItem[Boolean]("i6", "I prefer vanilla"))) ~
      Part(
        name = "p2",
        SimpleItem[Boolean]("i7", "I answered this questionnaire truthfully")))

  val r = Request(q._p1.group("i3"), Answer(q._p1._i3, 30))
  "Getting the answer for an item'" should "return its value" in {
    r.answer(q._p1._i3).get shouldBe (30)
  }
  
  //TODO: other validation tests

}