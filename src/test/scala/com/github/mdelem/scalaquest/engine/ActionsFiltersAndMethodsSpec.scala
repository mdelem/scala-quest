package com.github.mdelem.scalaquest.engine

import org.scalatest._

class ActionsFiltersAndMethodsSpec extends FlatSpec with Matchers {

  val q = Questionnaire(
    name = "q1",
    Seq(Part(
      name = "p1",
      Seq(ItemGroup(name = "ig1",
        Seq(SimpleItem[Int]("i1", "Age"),
          SimpleItem[String]("i2", "Gender"))),
        ItemGroup(name = "ig3",
          Seq(SimpleItem[Int]("i3", "I like chocolate"))),
        ItemGroup(name = "ig4",
          Seq(SimpleItem[Int]("i4", "I like vanilla"))),
        ItemGroup(name = "ig5",
          Seq(ComplexItem("ci1",
            Seq(SimpleItem[Boolean]("i5", "I prefer chocolate"),
              SimpleItem[Boolean]("i6", "I prefer vanilla"))))))),
      Part(
        name = "p2",
        Seq(ItemGroup(name = "ig6", Seq(SimpleItem[Boolean]("i7", "I answered this questionnaire truthfully")))))))

  val defaultActions = Actions(q)

  "The default get'" should "return a response containing the same questionnaire node of the request" in {
    defaultActions.service(Request(q.group("ig1"))) should be(Response(Some(q.group("ig1"))))
  }
  "The default post'" should "step in the questionnaire" in {
    val request = Request(q.group("ig1"),
      Seq(Answer(q.simpleItem("i1"), "40")))
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
    .whenIn(q.part("p1"))
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
    filteredActions.service(Request(q.part("p1"))).parameters("mesg") should be("BA")

    //Second part has only the questionnaire filter
    filteredActions.service(Request(q.part("p2"))).parameters("mesg") should be("A")
  }

  "Gets" should "not be chained" in {
    filteredActions.service(Request(q.part("p1"))).parameters.get("getA") should be(None)
    filteredActions.service(Request(q.part("p1"))).parameters.get("getB") should be(Some("true"))

    filteredActions.service(Request(q.part("p2"))).parameters.get("getA") should be(Some("true"))
    filteredActions.service(Request(q.part("p2"))).parameters.get("getB") should be(None)
  }

  "Posts" should "not be chained" in {
    filteredActions.service(Request(q.group("ig1"),
      Answer(q.simpleItem("i1"), "40"))).parameters.get("postA") should be(None)
    filteredActions.service(Request(q.group("ig1"),
      Answer(q.simpleItem("i1"), "40"))).parameters.get("postB") should be(Some("true"))

    filteredActions.service(Request(q.group("ig6"),
      Answer(q.simpleItem("i7"), "true"))).parameters.get("postA") should be(Some("true"))
    filteredActions.service(Request(q.group("ig6"),
      Answer(q.simpleItem("i7"), "true"))).parameters.get("postB") should be(None)

  }

}