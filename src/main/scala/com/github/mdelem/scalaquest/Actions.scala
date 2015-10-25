package com.github.mdelem.scalaquest

case class Request(answers: Map[SimpleItem[_], _]) {
  def answer[T](i : SimpleItem[T]) : T = {
    answers(i).asInstanceOf[T]
  }   
}

case class Response()

object Actions {
  def apply(q: Questionnaire): Actions = {
    new Actions(q)
  }
}

class Actions(q: Questionnaire) {

  def whenIn(part: Part): Actions = {
    this
  }

  def whenIn(itemGroup: ItemGroup): Actions = {
    this
  }

  def filter(m: (Request, Response, Actions) => Response): Actions = {
    this
  }

  def get(m: (Request, Actions) => Response): Actions = {
    this
  }

  def post(m: (Request, Actions) => Response): Actions = {
    this
  }

  def service(r: Request): Response = {
    Response()
  }

  def step(r: Request): Response = {
    Response()
  }

  def jumpTo(r: Request, part: Part): Response = {
    Response()
  }
  def jumpTo(r: Request, itemGroup: ItemGroup): Response = {
    Response()
  }

}