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
//    println(showRaw(inputAsTree))
//    wrap(c)(inputAsTree)

    inputAsTree match {
      case input @ ValDef(modifiers, name, _, a @ Apply(Ident(TermName("Questionnaire")), _)) =>
        val wrapperName = s"Questionnaire${input.name.encodedName.toString.capitalize}WithAccessors"

        val wrappedCode = ValDef(modifiers, name, Ident(TypeName(wrapperName)), a)

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

    t match {
      case Apply(Ident(TermName("Part")), (AssignOrNamedArg(Ident(TermName("name")),
        Literal(Constant(partName: String))) :: _)) => 
          partName +: t.children.flatMap(findPartNames(c)(_))
      case _ => 
        t.children.flatMap(findPartNames(c)(_))
    }
  }

  
//  private def wrap(c: Context)(t: c.Tree): Tree = {
//    import c.universe._
//
//    t match {
//      case ValDef(modifiers, valName, _, a @ Apply(Ident(TermName("Questionnaire")), children @ (AssignOrNamedArg(Ident(TermName("name")),
//        Literal(Constant(name: String))) :: _))) =>
//        println("In questionnaire: " + name)
//        children.foreach(wrap(c)(_))
//        println("--- end questionnaire")
//      case Apply(Ident(TermName("Part")), children @ (AssignOrNamedArg(Ident(TermName("name")),
//        Literal(Constant(name: String))) :: _)) =>
//        println("In part: " + name)
//        children.foreach(wrap(c)(_))
//        println("--- end part")
//      case Apply(Ident(TermName("ItemGroup")), children @ (AssignOrNamedArg(Ident(TermName("name")),
//        Literal(Constant(name: String))) :: _)) =>
//        println("In item group: " + name)
//        children.foreach(wrap(c)(_))
//        println("--- end group")
//      case Apply(Ident(TermName("ComplexItem")),
//        children @ Literal(Constant(name: String)) :: _) =>
//        println("In complex item: " + name)
//        children.foreach(wrap(c)(_))
//        println("--- end complex item")
//      case Apply(TypeApply(Ident(TermName("SimpleItem")),
//        List(Ident(TypeName(_type: String)))), children @ Literal(Constant(name: String)) :: _) =>
//        println("Simple item: " + (_type, name))
//      case t =>
//        //TODO: need to use a Transformer instead of pattern matching
//        t.children.foreach(listElements(c)(_))
//    }
//
//  }

}
