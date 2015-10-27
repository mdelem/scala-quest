package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._

class ActionsFiltersAndMethodsSpec extends FlatSpec with Matchers {

  val q = Questionnaire(
    name = "q1",
    Part(
      name = "p1",
      ItemGroup(
        SimpleItem[Int]("d1", "Age") ~
          SimpleItem[String]("d2", "Gender")) ~
        SimpleItem[Int](name = "likeChocolate", "I like chocolate") ~
        SimpleItem[Int](name = "likeVanilla", "I like vanilla") ~
        ComplexItem(
          SimpleItem[Boolean]("I prefer chocolate") ~
            SimpleItem[Boolean]("I prefer vanilla"))) ~
      Part(
        name = "p2",
        SimpleItem[Boolean]("I answered this questionnaire truthfully")))

  val defaultActions = Actions(q)

  "The default get'" should "return a response containing the same questionnaire node of the request" in {
    defaultActions.service(Request(q.parts.head.groups.head)) should be(Response(Some(q.parts.head.groups.head)))
  }
  "The default post'" should "step in the questionnaire" in {
    val request = Request(q.parts.head.groups.head,
      Map(q.parts.head.groups.head.items.head.asInstanceOf[SimpleItem[Int]] -> 40))
    defaultActions.service(request) should be(defaultActions.step(request))
  }

  val filteredActions = Actions(q)
    .filter { (request, actions) =>
      val response = actions.service(request)
      val mesg = response.parameters.get("mesg").getOrElse("")
      Response(response.node, response.parameters + ("mesg" -> (mesg + "A")))
    }.get { (request, actions) =>
      Response(Some(request.node), Map("getA" -> "true"))
    }.post { (request, actions) =>
      val response = actions.step(request)
      Response(response.node, response.parameters + ("postA" -> "true"))
    }
    .whenIn(q.parts(0))
    .filter { (request, actions) =>
      val response = actions.service(request)
      val mesg = response.parameters.get("mesg").getOrElse("")
      Response(response.node, response.parameters + ("mesg" -> (mesg + "B")))
    }.get { (request, actions) =>
      Response(Some(request.node), Map("getB" -> "true"))
    }.post { (request, actions) =>
      val response = actions.step(request)
      Response(response.node, response.parameters + ("postB" -> "true"))
    }

  "Filters'" should "be chained in order" in {
    //First part has two filters
    filteredActions.service(Request(q.parts.head)).parameters("mesg") should be("BA")

    //Second part has only the questionnaire filter
    filteredActions.service(Request(q.parts(1))).parameters("mesg") should be("A")
  }

  "Gets" should "not be chained" in {
    filteredActions.service(Request(q.parts.head)).parameters.get("getA") should be(None)
    filteredActions.service(Request(q.parts.head)).parameters.get("getB") should be(Some("true"))

    filteredActions.service(Request(q.parts(1))).parameters.get("getA") should be(Some("true"))
    filteredActions.service(Request(q.parts(1))).parameters.get("getB") should be(None)
  }

  "Posts" should "not be chained" in {
    filteredActions.service(Request(q.parts.head.groups.head,
      Map(q.parts.head.groups.head.items.head.asInstanceOf[SimpleItem[Int]] -> 40))).parameters.get("postA") should be(None)
    filteredActions.service(Request(q.parts.head.groups.head,
      Map(q.parts.head.groups.head.items.head.asInstanceOf[SimpleItem[Int]] -> 40))).parameters.get("postB") should be(Some("true"))

    filteredActions.service(Request(q.parts(1).groups.head,
      Map(q.parts(1).groups.head.items.head.asInstanceOf[SimpleItem[Boolean]] -> true))).parameters.get("postA") should be(Some("true"))
    filteredActions.service(Request(q.parts(1).groups.head,
      Map(q.parts(1).groups.head.items.head.asInstanceOf[SimpleItem[Boolean]] -> true))).parameters.get("postB") should be(None)

  }

}