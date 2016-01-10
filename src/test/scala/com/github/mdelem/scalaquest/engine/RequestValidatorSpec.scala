package com.github.mdelem.scalaquest

import org.scalatest._
import javax.xml.bind.ValidationException
import com.github.mdelem.scalaquest.engine.Request
import com.github.mdelem.scalaquest.engine.Answer
import com.github.mdelem.scalaquest.engine.SimpleItem
import com.github.mdelem.scalaquest.engine.Questionnaire
import com.github.mdelem.scalaquest.engine.Part
import com.github.mdelem.scalaquest.engine.ItemGroup
import com.github.mdelem.scalaquest.engine.ComplexItem
import com.github.mdelem.scalaquest.engine.Validator
import Validator.{ ValidatesInt, ValidatesString, ValidatesLong, ValidatesDouble }

class RequestValidatorSpec extends FlatSpec with Matchers {

  val q = Questionnaire(
    name = "q1",
    Seq(Part(
      name = "p1",
      Seq(ItemGroup(name = "ig1",
        Seq(SimpleItem[Long]("i1", "Age")(ValidatesLong(0, 140, 1)),
          SimpleItem[String]("i2", "Gender")(ValidatesString(Set("M", "F", "O"))))),
        ItemGroup(name = "ig3", Seq(SimpleItem[Int]("i3", "I like chocolate")(ValidatesInt(1, 10, 1)))),
        ItemGroup(name = "ig4", Seq(SimpleItem[Int]("i4", "I like vanilla")(ValidatesInt(1, 10, 1)))),
        ItemGroup(name = "ig5", Seq(ComplexItem("ci1",
          Seq(SimpleItem[Boolean]("i5", "I prefer chocolate"),
            SimpleItem[Boolean]("i6", "I prefer vanilla"))))))),
      Part(
        name = "p2",
        Seq(ItemGroup(name = "ig6", Seq(SimpleItem[Double]("i7", "I answered this questionnaire truthfully")(ValidatesDouble(0, 1))))))))

  "Getting the answer for an item'" should "return its value" in {
    val item = q.simpleItem("i3").asInstanceOf[SimpleItem[Int]]
    val answer = Answer(item, "10")
    val r = Request(q.group("ig3"), answer)
    r.answer(item).get shouldBe (10)
  }

  an[ValidationException] should be thrownBy {
    val item = q.simpleItem("i3").asInstanceOf[SimpleItem[Int]]
    val answer = Answer(item, "11")
    val r = Request(q.group("ig3"), answer)
    r.answer[Int](item).get
  }

  an[ValidationException] should be thrownBy {
    val item = q.simpleItem("i2").asInstanceOf[SimpleItem[String]]
    val answer = Answer(item, "Z")
    val r = Request(q.group("ig1"), answer)
    r.answer(item).get
  }

  an[ValidationException] should be thrownBy {
    val item = q.simpleItem("i1").asInstanceOf[SimpleItem[Long]]
    val answer = Answer(item, "-1")
    val r = Request(q.group("ig1"), answer)
    r.answer(item).get
  }

  an[ValidationException] should be thrownBy {
    val item = q.simpleItem("i7").asInstanceOf[SimpleItem[Double]]
    val answer = Answer(item, "Z")
    val r = Request(q.group("ig6"), answer)
    r.answer(item).get
  }

}