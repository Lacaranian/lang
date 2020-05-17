package com.schemagames.lang.interpreter

import com.schemagames.lang.interpreter.Interpreter.evaluateExpression
import com.schemagames.lang.parser.InterpreterError
import com.schemagames.lang.syntax.SyntaxTree
import com.schemagames.lang.syntax.SyntaxTree.{BlockExpression, Constant, Definition, Expression, Term, TermExpression, Variable, VariableTerm}

import scala.annotation.tailrec

case class Context(
  symbolTable: Map[String, Term] = Map()
)

object Interpreter {
  def apply(ast: List[SyntaxTree]): Either[InterpreterError, ()] = {
    // Build a symbol table from the AST, evaluating expressions as it goes
    val completeContext = ast.foldLeft(Context()){ case (context, syntaxTree) => syntaxTree match {
      case definition: Definition => recordDefinition(definition, context)
      case expr: Expression => {
        evaluateExpression(expr, context) // For side-effects?
        context
      }
    }}

    completeContext.symbolTable.get("main").map(mainTerm => {
      println(mainTerm)
      Right(())
    }).getOrElse({
      Left(InterpreterError("No main definition found"))
    })
  }

  def recordDefinition(definition: Definition, context: Context): Context = {
    val Definition(Variable(name), expr) = definition

    val term = evaluateExpression(expr, context)

    context.copy(symbolTable = context.symbolTable + (name -> term))
  }

  @tailrec
  def evaluateExpression(expr: Expression, context: Context): Term = expr match {
    case TermExpression(term) => evaluateTerm(term, context)
    case BlockExpression(definitions, expr) => {
      // The context and changes to it in this block, stay in this block (scoping)
      val blockContext = definitions.foldLeft(context) { case (latestContext, definition) => {
        recordDefinition(definition, latestContext)
      }}

      evaluateExpression(expr, blockContext)
    }
  }

  def evaluateTerm(term: Term, context: Context): Term = term match {
    case c: Constant => c
    // Perhaps substitute a variable's value in this context for its term
    case v @ VariableTerm(Variable(name)) => context.symbolTable.getOrElse(name, v)
  }
}
