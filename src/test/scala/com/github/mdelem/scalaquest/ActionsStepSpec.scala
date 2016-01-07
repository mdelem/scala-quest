package com.github.mdelem.scalaquest

import org.scalatest._

class ActionsStepSpec extends FlatSpec with Matchers {

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

  val actions = Actions(q)

  "The next step after the start of the questionnaire" should "be its first part" in {
    actions.step(Request(q)) should be(Response(Some(q.part("p1"))))
  }

  "The next step after part 'p1'" should "be its first item group" in {
    actions.step(Request(q.part("p1"))) should be(Response(Some(q.group("ig1"))))
  }

  "The next step after the last item group of part 'p1'" should "be part 'p2'" in {
    actions.step(Request(q.group("ig5"))) should be(Response(Some(q.part("p2"))))
  }

  "The next step after the last item of the questionnaire" should "be None" in {
    actions.step(Request(q.group("ig6"))) should be(Response(None))
  }

  "The next step after item 'i3'" should "be the next item group containing item 'i4'" in {
    actions.step(Request(q.group("ig3"))) should be(Response(Some(q.group("ig4"))))
  }

  val qRandom = Questionnaire(
    name = "q1",randomized = true,
    Seq(Part(
      name = "p1",randomized = true,
      Seq(ItemGroup(name = "ig1",randomized = true,
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

  val actionsRandom = Actions(qRandom)

  "The next step after the start of the randomized questionnaire" should "be a random part" in {
    actionsRandom.step(Request(qRandom, "SSID-999-222")) should be(Response(Some(qRandom.part("p1"))))
    actionsRandom.step(Request(qRandom, "SSID-999-111")) should be(Response(Some(qRandom.part("p2"))))
  }

  "The next step after the start of a randomized part" should "be a random item group" in {
    actionsRandom.step(Request(qRandom.part("p1"), "SSID-999-222")) should be(Response(Some(qRandom.group("ig5"))))
    actionsRandom.step(Request(qRandom.part("p1"), "SSID-999-111")) should be(Response(Some(qRandom.group("ig1"))))
  }

  "The next step after item 'i3'" should "be a random item group or None if the questionnaire is finished" in {
    actionsRandom.step(Request(qRandom.group("ig3"), "SSID-999-222")) should be(Response(Some(qRandom.group("ig1"))))
    actionsRandom.step(Request(qRandom.group("ig3"), "SSID-999-111")) should be(Response(None))
  }

  "The next step after the last item group of part 'p2'" should "be another random part (ie 'p1') or None if the questionnaire is finished" in {
    actionsRandom.step(Request(qRandom.group("ig6"), "SSID-999-222")) should be(Response(None))
    actionsRandom.step(Request(qRandom.group("ig6"), "SSID-999-111")) should be(Response(Some(qRandom.part("p1"))))
  }
}