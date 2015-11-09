package com.github.mdelem.scalaquest

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

@compileTimeOnly("enable macro paradise to expand macro annotations")
class WithAccessors extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro WithAccessorsMacro.impl
}

object WithAccessorsMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputAsTree = annottees.head.tree
    inputAsTree match {
      case input @ ValDef(modifiers, name, _, a @ Apply(Ident(TermName("Questionnaire")), _)) =>
        val (Some(wrapperName), wrapperCode) = createWrapper(c)(input)
        val valDef = ValDef(modifiers, name, Ident(TypeName(wrapperName)), a)
        val wrapperDefs = c.parse(wrapperCode)
        
        val output = Block(wrapperDefs.children :+ valDef, Literal(Constant(())))
//        println(showCode(output))
        c.Expr[Any](output)
      case _ =>
        c.abort(c.enclosingPosition, "Annotation can only be applied to Questionnaire declarations")
    }

  }

  def findPartNames(c: Context)(t: c.Tree): Seq[String] = {
    import c.universe._

    t match {
      case Apply(Ident(TermName("Part")), (AssignOrNamedArg(Ident(TermName("name")),
        Literal(Constant(partName: String))) :: _)) =>
        partName +: t.children.flatMap(findPartNames(c)(_))
      case _ =>
        t.children.flatMap(findPartNames(c)(_))
    }
  }

  private def createWrapper(c: Context)(t: c.Tree): (Option[String], String) = {
    import c.universe._

    t match {
      case Apply(Ident(TermName("Questionnaire")), children @ (AssignOrNamedArg(Ident(TermName("name")),
        Literal(Constant(name: String))) :: _)) =>
        createQuestionnaireWrapper(c)(name, children)
      case Apply(Ident(TermName("Questionnaire")), children @ (Literal(Constant(name: String)) :: _)) =>
        createQuestionnaireWrapper(c)(name, children)
      case Apply(Ident(TermName("Part")), children @ (AssignOrNamedArg(Ident(TermName("name")),
        Literal(Constant(name: String))) :: _)) =>
        createPartWrapper(c)(name, children)
      case Apply(Ident(TermName("Part")), children @ (Literal(Constant(name: String)) :: _)) =>
        createPartWrapper(c)(name, children)
      case Apply(Ident(TermName("ItemGroup")), children @ (AssignOrNamedArg(Ident(TermName("name")),
        Literal(Constant(name: String))) :: _)) =>
        createItemGroupWrapper(c)(name, children)
      case Apply(Ident(TermName("ItemGroup")), children @ (Literal(Constant(name: String)) :: _)) =>
        createItemGroupWrapper(c)(name, children)
      case Apply(Ident(TermName("ComplexItem")),
        children @ (AssignOrNamedArg(Ident(TermName("name")), Literal(Constant(name: String))) :: _)) =>
        createComplexItemWrapper(c)(name, children)
      case Apply(Ident(TermName("ComplexItem")),
        children @ (Literal(Constant(name: String)) :: _)) =>
        createComplexItemWrapper(c)(name, children)
      case Apply(TypeApply(Ident(TermName("SimpleItem")), List(Ident(TypeName(_type: String)))),
        children @ (AssignOrNamedArg(Ident(TermName("name")), Literal(Constant(name: String))) :: _ :: _)) =>
        createSimpleItemWrapper(c)(name, _type)
      case Apply(TypeApply(Ident(TermName("SimpleItem")), List(Ident(TypeName(_type: String)))),
        children @ (Literal(Constant(name: String)) :: _ :: _)) =>
        createSimpleItemWrapper(c)(name, _type)
      case t =>
        val childrenWrappers = t.children.map(createWrapper(c)(_))
        val wrapperName = childrenWrappers.collectFirst {
          case w @ (Some(wrapperName), _) => wrapperName
        }
        (wrapperName, childrenWrappers.map(_._2).mkString("\n"))
    }

  }

  def createSimpleItemWrapper(c: Context)(name: String, _type: String): (Option[String], String) = {
    import c.universe._
    val encoded = TermName(name).toString
    (None, s"""def _$encoded : SimpleItem[${_type}] = item("$name").asInstanceOf[SimpleItem[${_type}]]""")
  }

  def createComplexItemWrapper(c: Context)(name: String, children: List[c.Tree]): (Option[String], String) = {
    import c.universe._
    val encoded = TermName(name).toString
    val wrapperName = s"ComplexItem${encoded.capitalize}Wrapped"
    val body = children.map(createWrapper(c)(_)._2).mkString("\n")

    (Some(wrapperName), s"""class $wrapperName(name: String, randomized: Boolean = false, items: Seq[SimpleItem[_]])
      extends ComplexItem(name, randomized, items) {
      $body
    }
    implicit def to$wrapperName(ci: ComplexItem): $wrapperName = {
      new $wrapperName(ci.name, ci.randomized, ci.items)
    }
    def _$encoded : $wrapperName = complexItem("$name")
    """)
  }

  def createItemGroupWrapper(c: Context)(name: String, children: List[c.Tree]): (Option[String], String) = {
    import c.universe._
    val encoded = TermName(name).toString
    val wrapperName = s"ItemGroup${encoded.capitalize}Wrapped"
    val body = children.map(createWrapper(c)(_)._2).mkString("\n")

    (Some(wrapperName), s"""class $wrapperName(name: String, randomized: Boolean = false,  items: Seq[Item])
      extends ItemGroup(name, randomized, items) {
      $body
    }
    implicit def to$wrapperName(ig: ItemGroup): $wrapperName = {
      new $wrapperName(ig.name, ig.randomized, ig.items)
    }
    def _$encoded : $wrapperName = group("$name")
    """)
  }

  def createPartWrapper(c: Context)(name: String, children: List[c.Tree]): (Option[String], String) = {
    import c.universe._
    val encoded = TermName(name).toString
    val wrapperName = s"Part${encoded.capitalize}Wrapped"
    val body = children.map(createWrapper(c)(_)._2).mkString("\n")

    (Some(wrapperName), s"""class $wrapperName(name: String, randomized: Boolean = false, groups: Seq[ItemGroup])
      extends Part(name, randomized, groups) {
      $body
    }
    implicit def to$wrapperName(p: Part): $wrapperName = {
      new $wrapperName(p.name, p.randomized, p.groups)
    }
    def _$encoded : $wrapperName = part("$name")
    """)
  }

  def createQuestionnaireWrapper(c: Context)(name: String, children: List[c.Tree]): (Option[String], String) = {
    import c.universe._
    val encoded = TermName(name).toString
    val wrapperName = s"Questionnaire${encoded.capitalize}Wrapped"
    val body = children.map(createWrapper(c)(_)._2).mkString("\n")

    (Some(wrapperName), s"""class $wrapperName(name: String, randomized: Boolean = false, parts: Seq[Part])
          extends Questionnaire(name, randomized, parts) {
      $body
    }
    implicit def to$wrapperName(q: Questionnaire): $wrapperName = {
      new $wrapperName(q.name, q.randomized, q.parts)
    }
    """)
  }

}
