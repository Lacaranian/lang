package com.schemagames.lang.syntax

import com.schemagames.lang.interpreter.Context

import scala.util.parsing.input.Positional

sealed trait UntypedAST extends Positional

object UntypedAST {
  case class Definition(variable: VariableTerm, expression: Expression) extends UntypedAST

  sealed trait Expression extends UntypedAST
  case class BlockExpression(definitions: List[Definition], finalExpr: Expression) extends Expression

  sealed trait Term extends Expression
  case class VariableTerm(identifier: String, annotation: Option[Annotation] = None) extends Term

  sealed trait Constant extends Term // The type of constants is implied by them
  case class StringConstant(str: String) extends Constant
  case class NumberConstant(num: Int) extends Constant

  case class Application(applyingTerm: Expression, appliedTerm: Expression) extends Term
  case class Abstraction(variable: VariableTerm, expression: Expression) extends Term // Also captures quantification for Expressions of type Type (Π x: A. B(x))

  case class Annotation(typeExpr: Expression) extends Positional // The tree with the annotation is flagged as "of type <annotation.expression>" in a Typer/TypedInterpreter

  // sealed trait TermConstant extends Term // Also known as Sorts? * and BOX?
  // case object Kind extends TermConstant
}

