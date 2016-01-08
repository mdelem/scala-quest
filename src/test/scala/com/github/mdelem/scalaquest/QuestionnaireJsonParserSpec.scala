package com.github.mdelem.scalaquest

import org.scalatest._
import javax.xml.bind.ValidationException
import Validator.{ ValidatesInt, ValidatesString }

class QuestionnaireJsonParserSpec extends FlatSpec with Matchers {

  val q = QuestionnaireJsonParser.parse(getClass.getClassLoader().getResourceAsStream("questionnaire1.json"))

  "The name of the questionnaire" should "be 'q1'" in {
    q.name shouldBe ("q1")
  }

  "The questionnaire" should "have two parts" in {
    q.parts.size shouldBe (2)
  }

  "The questionnaire parts" should "not be randomized" in {
    q.randomized shouldBe (false)
  }

  "The name of the first part" should "be 'p1'" in {
    q.parts.head.name shouldBe ("p1")
  }
  
  "The first part" should "have four groups" in {
    q.parts.head.groups.size shouldBe (4)
  }
  
  "Complex item 'ci1'" should "have two sub items" in {
    q.complexItem("ci1").items.size shouldBe (2)
  }
  
  "Item 'i5' inside 'ci1'" should "have a proposition" in {
    q.complexItem("ci1").item("i5").proposition shouldBe ("I prefer chocolate")
  }
  
  "Item 'i4'" should "be a scale from 1 to 10" in {
    val v = q.simpleItem("i4").validator.asInstanceOf[ValidatesInt]
    v.min shouldBe (1)
    v.max shouldBe (10)
    v.step shouldBe (1)
  }
  
}