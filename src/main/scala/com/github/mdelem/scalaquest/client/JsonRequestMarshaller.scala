package com.github.mdelem.scalaquest.client

import com.github.mdelem.scalaquest.engine.Response
import com.github.mdelem.scalaquest.engine.Request
import play.api.libs.json._
import play.api.libs.json.Writes._
import play.api.libs.functional.syntax._
import com.github.mdelem.scalaquest.engine.QuestionnaireNode
import javax.xml.bind.ValidationException
import play.api.libs.json.jackson.JsValueDeserializer
import com.github.mdelem.scalaquest.engine.Questionnaire
import com.github.mdelem.scalaquest.engine.Part
import com.github.mdelem.scalaquest.engine.ItemGroup
import com.github.mdelem.scalaquest.engine.Item
import com.github.mdelem.scalaquest.engine.ComplexItem
import com.github.mdelem.scalaquest.engine.SimpleItem
import com.github.mdelem.scalaquest.engine.Validator.Validates
import com.github.mdelem.scalaquest.engine.Validator.ValidatesInt
import com.github.mdelem.scalaquest.engine.Validator.ValidatesLong
import com.github.mdelem.scalaquest.engine.Validator.ValidatesDouble
import com.github.mdelem.scalaquest.engine.Validator.ValidatesString
import com.github.mdelem.scalaquest.engine.Answer

object JsonRequestMarshaller {

  implicit val questionnaireWrites: Writes[Questionnaire] = (
    (JsPath \ "node_type").write[String] and
    (JsPath \ "name").write[String])((q: Questionnaire) => ("questionnaire", q.name))

  implicit val partWrites: Writes[Part] = (
    (JsPath \ "node_type").write[String] and
    (JsPath \ "name").write[String])((p: Part) => ("part", p.name))

  implicit val validatesIntWrites = Json.writes[ValidatesInt]
  implicit val validatesLongWrites = Json.writes[ValidatesLong]
  implicit val validatesDoubleWrites = Json.writes[ValidatesDouble]
  implicit val validatesStringWrites = Json.writes[ValidatesString]

  implicit object validatesWrites extends Writes[Validates[_]] {
    def writes(v: Validates[_]): JsValue = {
      v match {
        case v @ ValidatesInt(_, _, _)  => validatesIntWrites.writes(v)
        case v @ ValidatesLong(_, _, _) => validatesLongWrites.writes(v)
        case v @ ValidatesDouble(_, _)  => validatesDoubleWrites.writes(v)
        case v @ ValidatesString(_)     => validatesStringWrites.writes(v)
        case _                          => JsNull
      }
    }
  }

  implicit val simpleItemWrites: Writes[SimpleItem[_]] = (
    (JsPath \ "node_type").write[String] and
    (JsPath \ "name").write[String] and
    (JsPath \ "proposition").write[String] and
    (JsPath \ "validator").write[Option[Validates[_]]])((s: SimpleItem[_]) => {
      s.validator match {
        case v @ ValidatesInt(_, _, _)  => ("simpleitem", s.name, s.proposition, Some(s.validator))
        case v @ ValidatesLong(_, _, _) => ("simpleitem", s.name, s.proposition, Some(s.validator))
        case v @ ValidatesDouble(_, _)  => ("simpleitem", s.name, s.proposition, Some(s.validator))
        case v @ ValidatesString(_)     => ("simpleitem", s.name, s.proposition, Some(s.validator))
        case _                          => ("simpleitem", s.name, s.proposition, None)
      }

    })

  implicit val complexItemWrites: Writes[ComplexItem] = (
    (JsPath \ "node_type").write[String] and
    (JsPath \ "name").write[String] and
    (JsPath \ "proposition").write[Option[String]] and
    (JsPath \ "items").write[Seq[Item]])((s: ComplexItem) => ("complexitem", s.name, s.proposition, s.items))

  implicit object itemWrites extends Writes[Item] {
    def writes(i: Item): JsValue = {
      i match {
        case c @ ComplexItem(_, _, _, _) => complexItemWrites.writes(c)
        case s @ SimpleItem(_, _, _)     => simpleItemWrites.writes(s)
      }
    }
  }

  implicit val groupWrites: Writes[ItemGroup] = (
    (JsPath \ "node_type").write[String] and
    (JsPath \ "name").write[String] and
    (JsPath \ "name").write[Seq[Item]])((g: ItemGroup) => ("itemgroup", g.name, g.items))

  implicit object nodeWrites extends Writes[QuestionnaireNode] {
    def writes(n: QuestionnaireNode): JsValue = {
      n match {
        case q @ Questionnaire(_, _, _) => questionnaireWrites.writes(q)
        case p @ Part(_, _, _)          => partWrites.writes(p)
        case g @ ItemGroup(_, _, _)     => groupWrites.writes(g)
      }
    }
  }

  implicit val responseWrites = Json.writes[Response]

  def toJson(r: Response): String = {
    Json.toJson(r).toString()
  }

  case class JsonRequest(node_name: String, answers: Map[String, String])
  implicit val jsonRequestReads = Json.reads[JsonRequest]

  def fromJson(input: String, q: Questionnaire, sessionId: String): Request = {
    val r = Json.fromJson[JsonRequest](Json.parse(input)).get
    val node: QuestionnaireNode = q.group.getOrElse(r.node_name,
      q.part.getOrElse(r.node_name, q))
    Request(node, r.answers.map {
      case (name, value) =>
        Answer(q.simpleItem(name), value)
    }.toSeq, sessionId)

  }

}