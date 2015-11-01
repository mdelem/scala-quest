package com.github.mdelem.scalaquest

import com.github.mdelem.scalaquest.Validator.Validates

case class Answer[T](item: SimpleItem[T], value: T)

case class Request(node: QuestionnaireNode, answers: Seq[Answer[_]] = Seq(), sessionId: String = "1") {
  def answer[T](i: SimpleItem[T])(implicit v: Validates[T]): Option[T] = {
    answers.find(_.item == i).map(a => v.validate(a.item.asInstanceOf[SimpleItem[T]], a.value.asInstanceOf[T]))
  }
}
object Request {
  def apply(node: QuestionnaireNode, sessionId: String): Request = new Request(node, Seq(), sessionId)
  def apply(node: QuestionnaireNode, answers: Seq[Answer[_]]): Request =
    new Request(node, answers)
  def apply(node: QuestionnaireNode, answer: Answer[_]): Request =
    new Request(node, Seq(answer))
}

case class Response(node: Option[QuestionnaireNode], parameters: Map[String, String])
object Response {
  def apply(node: Option[QuestionnaireNode]): Response = {
    Response(node, Map())
  }
}

object Actions {
  type Filter = (Request, Actions) => Response
  type Method = (Request, Actions) => Response

  def apply(q: Questionnaire): Actions = {
    val defaultGet = ((r: Request, a: Actions) => Response(Some(r.node)))
    val defaultPost = ((r: Request, a: Actions) => a.step(r))
    new Actions(q, q, Map(), Map(q -> defaultGet), Map(q -> defaultPost))
  }

  private def apply(q: Questionnaire, n: QuestionnaireNode): Actions = {
    new Actions(q, n, Map(), Map(), Map())
  }

  private def apply(q: Questionnaire, n: QuestionnaireNode, filters: Map[QuestionnaireNode, Filter],
                    gets: Map[QuestionnaireNode, Method], posts: Map[QuestionnaireNode, Method]): Actions = {
    new Actions(q, n, filters, gets, posts)
  }
}

import Actions._
class Actions(q: Questionnaire, n: QuestionnaireNode, filters: Map[QuestionnaireNode, Filter],
              gets: Map[QuestionnaireNode, Method], posts: Map[QuestionnaireNode, Method]) {

  def whenIn(n: QuestionnaireNode): Actions = {
    Actions(q, n, filters, gets, posts)
  }

  def filter(m: Filter): Actions = {
    Actions(q, n, filters + (n -> m), gets, posts)
  }

  def get(m: Method): Actions = {
    Actions(q, n, filters, gets + (n -> m), posts)
  }

  def post(m: Method): Actions = {
    Actions(q, n, filters, gets, posts + (n -> m))
  }

  def service(r: Request): Response = {
    val ancestors = r.node match {
      case q: Questionnaire => Seq(q)
      case p: Part          => Seq(q, p)
      case g: ItemGroup     => Seq(q, q.parent(g), g)
    }

    val f = ancestors.collectFirst({ case node if filters.isDefinedAt(node) => (node, filters(node)) })
    f match {
      case Some((node, filter)) => filter(r, Actions(q, n, filters - node, gets, posts))
      case None =>
        if (r.answers.isEmpty) {
          //is a GET
          val (node, getMethod) = ancestors.reverse.collectFirst({ case node if gets.isDefinedAt(node) => (node, gets(node)) }).get
          getMethod(r, this)
        } else {
          //is a POST
          val (node, postMethod) = ancestors.reverse.collectFirst({ case node if posts.isDefinedAt(node) => (node, posts(node)) }).get
          postMethod(r, this)
        }
    }

  }

  def step(r: Request): Response = {
    r.node match {
      case q: Questionnaire => Response(inOrder(q.parts, q.randomized, r.sessionId).headOption)
      case p: Part =>
        val g = inOrder(p.groups, p.randomized, r.sessionId).headOption
        if (g.isDefined)
          Response(g)
        else
          Response(getNextPart(p, r))
      case g: ItemGroup =>
        val next = getNextItemGroup(g, r)
        if (next.isDefined)
          Response(next)
        else
          Response(getNextPart(q.parent(g), r))

    }
  }

  private def getNextPart(previous: Part, r: Request): Option[Part] = {
    val partsInOrder = inOrder(q.parts, q.randomized, r.sessionId)
    val nextIndex = partsInOrder.indexOf(previous) + 1
    if (partsInOrder.isDefinedAt(nextIndex))
      Some(partsInOrder(nextIndex))
    else None
  }

  private def getNextItemGroup(previous: ItemGroup, r: Request): Option[ItemGroup] = {
    val p : Part = q.parent(previous)
    val groupsInOrder = inOrder(p.groups, q.randomized, r.sessionId)
    val nextIndex = groupsInOrder.indexOf(previous) + 1
    if (groupsInOrder.isDefinedAt(nextIndex))
      Some(groupsInOrder(nextIndex))
    else None
  }

  private def inOrder[T](s: Seq[T], randomized: Boolean, seed: String): Seq[T] = {
    if (randomized) {
      val r = util.Random
      r.setSeed(seed.hashCode())
      r.shuffle(s)
    } else {
      s
    }
  }

  def jumpTo(node: QuestionnaireNode): Response = {
    service(Request(node))
  }

}