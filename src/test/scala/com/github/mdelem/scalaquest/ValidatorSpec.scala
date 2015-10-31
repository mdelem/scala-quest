package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._

class ValidatorSpec extends FlatSpec with Matchers {

  val q = Questionnaire(
    name = "q1",
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

  val r = Request(q.group("ig1"), Answer(q.simpleItem("i3"), 30))
  "The default get'" should "return a response containing the same questionnaire node of the request" in {
    val value = r.answer[Int](q.simpleItem("i3")).get
    value shouldBe(30)
  }

}