package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._

class ActionsStepSpec extends FlatSpec with Matchers {

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
        SimpleItem[Boolean]("i7","I answered this questionnaire truthfully")))

  val actions = Actions(q)

  "The next step after the start of the questionnaire" should "its first part" in {
    actions.step(Request(q)) should be(Response(Some(q.part("p1"))))
  }

  "The next step after part 'p1'" should "its first item group" in {
    actions.step(Request(q.part("p1"))) should be(Response(Some(q.group("ig1"))))
  }

  "The next step after the last item group of part 'p1'" should "be part 'p2'" in {
    actions.step(Request(q.group("ci1"))) should be(Response(Some(q.part("p2"))))
  }

  "The next step after the last item of the questionnaire" should "be None" in {
    actions.step(Request(q.item("i7"))) should be(Response(None))
  }

  "The next step after item 'i3'" should "be the next item group containing item 'i4'" in {
    actions.step(Request(q.item("i3"))) should be(Response(Some(q.group("i4"))))
  }

}