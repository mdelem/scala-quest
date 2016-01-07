package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Validator.Validates
import javax.xml.bind.ValidationException

class RequestValidatorSpec extends FlatSpec with Matchers {

  case class Scale1_10(value: Int)
  implicit object ValidatesScale1_10 extends Validates[Scale1_10] {
    override def validate(i: SimpleItem[Scale1_10], x: Scale1_10): Scale1_10 = {
      if (x.value >= 1 && x.value <= 10)
        x
      else
        throw new ValidationException(s"Invalid value: $x; Must be an integer between 1 and 10.")
    }
  }

  val q = Questionnaire(
    name = "q1",
    Seq(Part(
      name = "p1",
      Seq(ItemGroup(name = "ig1",
        Seq(SimpleItem[Int]("i1", "Age"),
          SimpleItem[String]("i2", "Gender"))),
        ItemGroup(name = "ig3", Seq(SimpleItem[Scale1_10]("i3", "I like chocolate"))),
        ItemGroup(name = "ig4", Seq(SimpleItem[Scale1_10]("i4", "I like vanilla"))),
        ItemGroup(name = "ig5", Seq(ComplexItem("ci1",
          Seq(SimpleItem[Boolean]("i5", "I prefer chocolate"),
            SimpleItem[Boolean]("i6", "I prefer vanilla"))))))),
      Part(
        name = "p2",
        Seq(ItemGroup(name = "ig6", Seq(SimpleItem[Boolean]("i7", "I answered this questionnaire truthfully")))))))

  val item = q.simpleItem("i3")
  val answer = Answer(item, Scale1_10(10))
  val r = Request(q.group("ig3"), answer)
  "Getting the answer for an item'" should "return its value" in {
    r.answer[Scale1_10](item).get shouldBe (Scale1_10(10))
  }

  //TODO: other validation tests

}