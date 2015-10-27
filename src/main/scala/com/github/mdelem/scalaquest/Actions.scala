package com.github.mdelem.scalaquest

case class Request(node: QuestionnaireNode, answers: Map[SimpleItem[_], _]) {
  def answer[T](i: SimpleItem[T]): T = {
    answers(i).asInstanceOf[T]
  }
}
object Request {
  def apply(node: QuestionnaireNode): Request = {
    Request(node, Map())
  }
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
    //TODO: handle randomization
    r.node match {
      case q: Questionnaire => Response(q.parts.headOption)
      case p: Part =>
        val g = p.groups.headOption
        if (g.isDefined)
          Response(g)
        else
          Response(getNextPart(p))
      case g: ItemGroup =>
        val next = getNextItemGroup(g)
        if (next.isDefined)
          Response(next)
        else
          Response(getNextPart(q.parent(g).asInstanceOf[Part]))

    }
  }

  private def getNextPart(previous: Part): Option[Part] = {
    val nextIndex = q.parts.indexOf(previous) + 1
    if (q.parts.isDefinedAt(nextIndex))
      Some(q.parts(nextIndex))
    else None
  }

  private def getNextItemGroup(previous: ItemGroup): Option[ItemGroup] = {
    val p = q.parent(previous).asInstanceOf[Part]
    val nextIndex = p.groups.indexOf(previous) + 1
    if (p.groups.isDefinedAt(nextIndex))
      Some(p.groups(nextIndex))
    else None
  }

  def jumpTo(node: QuestionnaireNode): Response = {
    service(Request(node))
  }

}