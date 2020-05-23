package com.schemagames.lang.syntax

import com.schemagames.lang.interpreter.Context

import scala.util.parsing.input.Positional

sealed trait UntypedAST extends Positional

object UntypedAST {
  case class Definition(variable: Variable, expression: Expression) extends UntypedAST

  case class Variable(identifier: String) extends Positional

  sealed trait Expression extends UntypedAST
  case class BlockExpression(definitions: List[Definition], finalExpr: Expression) extends Expression

  sealed trait Term extends Expression
  case class VariableTerm(variable: Variable) extends Term

  sealed trait Constant extends Term
  case class StringConstant(str: String) extends Constant
  case class NumberConstant(num: Int) extends Constant

  case class Application(applyingTerm: Expression, appliedTerm: Expression) extends Term
  case class Abstraction(variable: Variable, expression: Expression) extends Term


  //case class Quantification() extends Term
  // sealed trait TermConstant extends Term // Also known as Sorts? * and BOX?
  // case object Kind extends TermConstant
}
