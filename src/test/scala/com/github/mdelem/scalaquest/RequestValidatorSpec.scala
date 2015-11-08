package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._
import com.github.mdelem.scalaquest.Validator.Validates
import javax.xml.bind.ValidationException

class RequestValidatorSpec extends FlatSpec with Matchers {

  case class Scale1_10(value: Int)
  implicit object ValidatesScale1_10 extends Validates[Scale1_10] {
    override def validate(i: SimpleItem[Scale1_10], x: Scale1_10): Scale1_10 = {
      if (x.value >= 1 && x.value <= 10)
        x
      else
        throw new ValidationException(s"Invalid value: $x; Must be an integer between 1 and 10.}")
    }
  }

  @WithAccessors
  val q = Questionnaire(
    "q1",
    Part(
      name = "p1",
      ItemGroup(name = "ig1",
        SimpleItem[Int]("i1", "Age") ~
          SimpleItem[String]("i2", "Gender", Seq("Male", "Female", "Other"))) ~
        SimpleItem[Scale1_10]("i3", "I like chocolate") ~
        SimpleItem[Scale1_10]("i4", "I like vanilla") ~
        ComplexItem("ci1",
          SimpleItem[Boolean]("i5", "I prefer chocolate") ~
            SimpleItem[Boolean]("i6", "I prefer vanilla"))) ~
      Part(
        name = "p2",
        SimpleItem[Boolean]("i7", "I answered this questionnaire truthfully")))

  val item = q._p1._i3
  val answer = Answer(item, Scale1_10(10))
  val r = Request(q._p1.group("i3"), answer)
  "Getting the answer for an item'" should "return its value" in {
    r.answer(item).get shouldBe (Scale1_10(10))
  }

  //TODO: other validation tests

}