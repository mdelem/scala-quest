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
      case input @ ValDef(_, _, _, Apply(Ident(TermName("Questionnaire")), _)) =>
        val wrapperName = s"Questionnaire${input.name.encodedName.toString.capitalize}WithAccessors"

        val wrappedCode = ValDef(Modifiers(), input.name, Ident(TypeName(wrapperName)),
          input.children(1))

        val accessors = findPartNames(c)(input)
          .map(partName => s"""def _$partName = this.part("$partName")""")
          .mkString("\n")
        val wrapperDef = c.parse(
          s"""
      class $wrapperName(name: Option[String] = None, randomized: Boolean = false, parts: Seq[Part])
          extends Questionnaire(name, randomized, parts) {
        $accessors
      }
      object $wrapperName {
        implicit def to$wrapperName(q: Questionnaire): $wrapperName = {
          new $wrapperName(q.name, q.randomized, q.parts)
        }
      }
      import $wrapperName._
      """)

        val output = Block(wrapperDef.children :+ wrappedCode, Literal(Constant(())))
        c.Expr[Any](output)
      case _ =>
        c.abort(c.enclosingPosition, "Annotation can only be applied to Questionnaire declarations")
    }

  }

  def findPartNames(c: Context)(t: c.Tree): Seq[String] = {
    import c.universe._

    val maybePartName = t match {
      case Apply(Ident(TermName("Part")), (AssignOrNamedArg(Ident(TermName("name")),
        Literal(Constant(partName: String))) :: _)) => Some(partName)
      case _ => None
    }

    (maybePartName ++ t.children.flatMap(findPartNames(c)(_))).toSeq
  }

}
