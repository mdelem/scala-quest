package com.github.mdelem.scalaquest

import org.scalatest._
import com.github.mdelem.scalaquest.Implicits._

class ActionsSpec extends FlatSpec with Matchers {

  val q = Questionnaire(
    name = "q1",
    Part(
      ItemGroup(
        SimpleItem[Int]("d1", "Age") ~
          SimpleItem[String]("d2", "Gender")) ~
        SimpleItem[Int]("I like chocolate") ~
        SimpleItem[Int]("I like vanilla") ~
        ComplexItem(
          SimpleItem[Boolean]("I prefer chocolate") ~
            SimpleItem[Boolean]("I prefer vanilla"))))

  val actions = Actions(q)
    .filter { (request, response, actions) =>
      actions.service(request)
    }.get { (request, actions) =>
      actions.service(request)
    }.post { (request, actions) =>
      //save answer to database here
      if (request.answer(q.parts(0).groups(1).items(0).asInstanceOf[SimpleItem[Int]]) > 5)
        actions.step(request)
      else
        actions.jumpTo(request, q.parts(0).groups(1))
    }

    .whenIn(q.parts(0).groups(0))
    .filter { (request, response, actions) =>
      actions.service(request)
    }

}