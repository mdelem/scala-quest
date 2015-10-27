package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._

class ActionsStepSpec extends FlatSpec with Matchers {

  val q = Questionnaire(
    name = "q1",
    Part(
      name = "p1",
      ItemGroup(
        SimpleItem[Int]("d1", "Age") ~
          SimpleItem[String]("d2", "Gender")) ~
        SimpleItem[Int](name="likeChocolate", "I like chocolate") ~
        SimpleItem[Int](name="likeVanilla", "I like vanilla") ~
        ComplexItem(
          SimpleItem[Boolean]("I prefer chocolate") ~
            SimpleItem[Boolean]("I prefer vanilla"))) ~
      Part(
        name = "p2",
        SimpleItem[Boolean]("I answered this questionnaire truthfully")))

  val actions = Actions(q)

  "The next step after the start of the questionnaire" should "its first part" in {
    actions.step(Request(q, Map())) should be(Response(Some(q.parts.head)))
  }
  
  "The next step after part 'p1'" should "its first item group" in {
    actions.step(Request(q.parts.head, Map())) should be(Response(Some(q.parts.head.groups.head)))
  }
  
  "The next step after the last item group of part 'p1'" should "be part 'p2'" in {
    actions.step(Request(q.parts.head.groups(3), Map())) should be(Response(Some(q.parts(1))))
  }
  
  "The next step after the last item group of the questionnaire" should "be None" in {
    actions.step(Request(q.parts(1).groups(0), Map())) should be(Response(None))
  }
  
  "The next step after item group 'likeChocolate'" should "be the next item group 'likeVanilla'" in {
    actions.step(Request(q.parts.head.groups(1), Map())) should be(Response(Some(q.parts.head.groups(2))))
  }

}