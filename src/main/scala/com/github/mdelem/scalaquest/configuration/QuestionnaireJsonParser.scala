package com.github.mdelem.scalaquest.configuration

import play.api.libs.json._
import java.io.InputStream
import scala.collection.mutable
import javax.xml.bind.ValidationException
import com.github.mdelem.scalaquest.engine.Validator._
import com.github.mdelem.scalaquest.engine.ComplexItem
import com.github.mdelem.scalaquest.engine.Item
import com.github.mdelem.scalaquest.engine.ItemGroup
import com.github.mdelem.scalaquest.engine.Part
import com.github.mdelem.scalaquest.engine.Questionnaire
import com.github.mdelem.scalaquest.engine.SimpleItem
import play.api.libs.json.JsValue.jsValueToJsLookup

object QuestionnaireJsonParser {
  def parse(inputStream: InputStream): Questionnaire = {
    val ast = Json.parse(inputStream)
    parseQuestionnaire(ast)
  }

  private def parseQuestionnaire(ast: JsValue): Questionnaire = {
    val name = (ast \ "name").as[String]
    val randomized = (ast \ "randomized").asOpt[Boolean]
    val jsParts = (ast \ "parts").as[JsArray].value

    var names = mutable.Set(name)
    val parts = for (jsPart <- jsParts)
      yield parsePart(jsPart, names)

    Questionnaire(name = name, randomized.getOrElse(false), parts)
  }

  private def parsePart(ast: JsValue, names: mutable.Set[String]): Part = {
    val name = (ast \ "name").as[String]
    if (names.contains(name))
      throw new ValidationException(s"Name ${name} is not unique")

    val randomized = (ast \ "randomized").asOpt[Boolean]
    val jsGroups = (ast \ "groups").as[JsArray].value

    val groups = for (jsGroup <- jsGroups)
      yield parseGroup(jsGroup, names)

    Part(name = name, randomized.getOrElse(false), groups)

  }

  private def parseGroup(ast: JsValue, names: mutable.Set[String]): ItemGroup = {
    val name = (ast \ "name").as[String]
    if (names.contains(name))
      throw new ValidationException(s"Name ${name} is not unique")

    val randomized = (ast \ "randomized").asOpt[Boolean]
    val jsItems = (ast \ "items").as[JsArray].value

    val items = for (jsItem <- jsItems)
      yield parseItem(jsItem, names)

    ItemGroup(name = name, randomized.getOrElse(false), items)

  }

  private def parseItem(ast: JsValue, names: mutable.Set[String]): Item = {
    if ((ast \ "items").toOption.isDefined)
      parseComplexItem(ast, names)
    else
      parseSimpleItem(ast, names)
  }

  private def parseComplexItem(ast: JsValue, names: mutable.Set[String]): ComplexItem = {
    val name = (ast \ "name").as[String]
    if (names.contains(name))
      throw new ValidationException(s"Name ${name} is not unique")

    val randomized = (ast \ "randomized").asOpt[Boolean]
    val proposition = (ast \ "proposition").asOpt[String]
    val jsItems = (ast \ "items").as[JsArray].value

    val items = for (jsItem <- jsItems)
      yield parseSimpleItem(jsItem, names)

    ComplexItem(name, proposition, randomized.getOrElse(false), items)

  }

  private def parseSimpleItem(ast: JsValue, names: mutable.Set[String]): SimpleItem[_] = {
    val name = (ast \ "name").as[String]
    if (names.contains(name))
      throw new ValidationException(s"Name ${name} is not unique")

    val proposition = (ast \ "proposition").as[String]
    val reversed = (ast \ "reversed").asOpt[Boolean]

    val itemType = (ast \ "type").as[String]

    val validatorLookup = (ast \ "validator")
    itemType match {
      case "Int" =>
        if (validatorLookup.toOption.isDefined)
          SimpleItem[Int](name, proposition, reversed.getOrElse(false))(parseIntValidator(validatorLookup.get))
        else
          SimpleItem[Int](name, proposition, reversed.getOrElse(false))
      case "Long" =>
        if (validatorLookup.toOption.isDefined)
          SimpleItem[Long](name, proposition, reversed.getOrElse(false))(parseLongValidator(validatorLookup.get))
        else
          SimpleItem[Long](name, proposition, reversed.getOrElse(false))
      case "Double" =>
        if (validatorLookup.toOption.isDefined)
          SimpleItem[Double](name, proposition, reversed.getOrElse(false))(parseDoubleValidator(validatorLookup.get))
        else
          SimpleItem[Double](name, proposition, reversed.getOrElse(false))
      case "String" =>
        if (validatorLookup.toOption.isDefined)
          SimpleItem[String](name, proposition, reversed.getOrElse(false))(parseStringValidator(validatorLookup.get))
        else
          SimpleItem[String](name, proposition, reversed.getOrElse(false))
      case "Boolean" =>
        SimpleItem[Int](name, proposition, reversed.getOrElse(false))
      case "DateTime" =>
        SimpleItem[Int](name, proposition, reversed.getOrElse(false))
      case t @ _ =>
        throw new ValidationException(s"Unknown item type: ${t}")
    }

  }

  private def parseIntValidator(ast: JsValue): ValidatesInt = {
    val min = (ast \ "min").asOpt[Int]
    val max = (ast \ "max").asOpt[Int]
    val step = (ast \ "step").asOpt[Int]
    ValidatesInt(min.getOrElse(Int.MinValue), max.getOrElse(Int.MaxValue), step.getOrElse(1))
  }
  
  private def parseLongValidator(ast: JsValue): ValidatesLong = {
    val min = (ast \ "min").asOpt[Long]
    val max = (ast \ "max").asOpt[Long]
    val step = (ast \ "step").asOpt[Long]
    ValidatesLong(min.getOrElse(Long.MinValue), max.getOrElse(Long.MaxValue), step.getOrElse(1L))
  }
  
  private def parseDoubleValidator(ast: JsValue): ValidatesDouble = {
    val min = (ast \ "min").asOpt[Double]
    val max = (ast \ "max").asOpt[Double]
    ValidatesDouble(min.getOrElse(Double.MinValue), max.getOrElse(Double.MaxValue))
  }
  
  private def parseStringValidator(ast: JsValue): ValidatesString = {
    val values = (ast \ "values").as[Array[String]]
    ValidatesString(values.toSet)
  }
}