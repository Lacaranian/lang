package com.schemagames.lang.interpreter

import com.schemagames.lang.InterpreterError
import com.schemagames.lang.compiler.Phase
import com.schemagames.lang.syntax.UntypedAST
import com.schemagames.lang.syntax.UntypedAST._

import scala.annotation.tailrec

case class Context(
  symbolTable: Map[String, Term] = Map()
)

case object Interpreter extends Phase[List[UntypedAST], UntypedAST.Term, InterpreterError] {
  def apply(ast: List[UntypedAST]): Either[InterpreterError, UntypedAST.Term] = {
    // Build a symbol table from the AST, evaluating expressions as it goes
    val completeContext = ast.foldLeft(Context()){ case (context, syntaxTree) => syntaxTree match {
      case definition: Definition => recordDefinition(definition, context)
      case expr: Expression => {
        evaluateExpression(expr, context) // For side-effects?
        context
      }
    }}

    completeContext.symbolTable.get("main").map(mainTerm => {
      Right(mainTerm)
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
    case term: Term => evaluateTerm(term, context)
    case BlockExpression(definitions, expr) => {
      // The context and changes to it in this block, stay in this block (scoping)
      val bContext = blockContext(definitions, context)

      evaluateExpression(expr, bContext)
    }
  }

  def evaluateTerm(term: Term, context: Context): Term = term match {
    case c: Constant => c
    // Perhaps substitute a variable's value in this context for its term
    case v @ VariableTerm(Variable(name)) => context.symbolTable.getOrElse(name, v)
    case l: Abstraction => l
    case Application(applyingExpr, appliedExpr) => {
      // Strict style, evaluate both the halves before applying them
      val applyingTerm = evaluateExpression(applyingExpr, context)
      val appliedTerm = evaluateExpression(appliedExpr, context)

      applyingTerm match {
        case Abstraction(paramVariable, lambdaBody) => {
          val contextWithLambdaParamFree = context.copy(symbolTable = context.symbolTable - paramVariable.identifier)
          val substitutedLambdaBody = substituteFreeOccurrencesInExpression(paramVariable, lambdaBody, appliedTerm, contextWithLambdaParamFree)

          // β-reduction
          evaluateExpression(substitutedLambdaBody, context)
        }
        case at => Application(at, appliedTerm)
      }
    }
  }

  def substituteFreeOccurrencesInTerm(
    variable: Variable,
    term: Term,
    replaceWith: Term,
    context: Context
  ): Term = term match {
    case c: Constant => c
    case VariableTerm(termVariable) if termVariable == variable => replaceWith
    case v: VariableTerm => v
    case Abstraction(paramVariable, lambdaBody) if paramVariable != variable => {
      val innerLambdaContext = context.copy(symbolTable = context.symbolTable - paramVariable.identifier)

      // Ensure paramVariable is not in the free variables of replaceWith (capture avoiding substitution)
      val replaceFreeVars = freeVariablesOfTerm(replaceWith, context)
      val safeReplaceWith = if(replaceFreeVars(paramVariable)) {
        val safeParamVar = Variable(paramVariable.identifier + "'") // Just prime it (is this enough?)
        substituteFreeOccurrencesInTerm(paramVariable, replaceWith, VariableTerm(safeParamVar), context)
      } else {
        replaceWith
      }

      val substitutedBody = substituteFreeOccurrencesInExpression(variable, lambdaBody, safeReplaceWith, innerLambdaContext)

      Abstraction(paramVariable, substitutedBody)
    }
    case l: Abstraction => l // variable is not "free" in the lambda
    case Application(left, right) => Application(
      substituteFreeOccurrencesInExpression(variable, left, replaceWith, context),
      substituteFreeOccurrencesInExpression(variable, right, replaceWith, context)
    )
  }

  def substituteFreeOccurrencesInExpression(
    variable: Variable,
    expression: Expression,
    replaceWith: Term,
    context: Context
  ): Expression = expression match {
    case term: Term => substituteFreeOccurrencesInTerm(variable, term, replaceWith, context)
    case BlockExpression(definitions, finalTerm) => {
      val (substitutedDefinitions, newContext) = definitions.foldLeft(List.empty[Definition] -> context) {
        case ((curDefs, curContext), d@Definition(name, body)) if name == variable || context.symbolTable.contains(name.identifier) => {
          // - This definition shadows the variable from here on out, so it is no longer free, or
          // - A previous definition in this scope shadowed the variable, so it is no longer free
          (curDefs :+ d, recordDefinition(d, curContext))
        }
        // This definition
        case ((curDefs, curContext), nextDef) => {
          val substitutedDefinition = nextDef.copy(expression =
            substituteFreeOccurrencesInExpression(variable, nextDef.expression, replaceWith, curContext)
          )
          (curDefs :+ substitutedDefinition, recordDefinition(substitutedDefinition, curContext))
        }
      }
      val substitutedFinalTerm = substituteFreeOccurrencesInExpression(variable, finalTerm, replaceWith, newContext)

      BlockExpression(substitutedDefinitions, substitutedFinalTerm)
    }
  }

  def freeVariablesOfTerm(term: Term, context: Context): Set[Variable] = (term match {
    case _: Constant                      => Set.empty[Variable]
    // Variables are free as long as they aren't bound in the context
    case VariableTerm(v @ Variable(name)) => Set(v)
    // Abstractions have all free variables of the body, minus the parameter (which binds it)
    case Abstraction(param, body)         => freeVariablesOfExpression(body, context) - param
    // Applications have all the free variables of both terms
    case Application(left, right)         => freeVariablesOfExpression(left, context) ++ freeVariablesOfExpression(right, context)
  }).filterNot(v =>
    // Anything bound in the context is not free
    context.symbolTable.contains(v.identifier)
  )


  def freeVariablesOfExpression(expression: Expression, context: Context): Set[Variable] = expression match {
    case term: Term => freeVariablesOfTerm(term, context)
    case BlockExpression(definitions, finalExpr) => {
      // In BlockExpressions, variables appear free until they are bound in a definition
      // From the outside, we only consider a variable fully free if it is never bound this way
      val bContext: Context = blockContext(definitions, context)

      freeVariablesOfExpression(finalExpr, bContext)
    }
  }

  def blockContext(definitions: List[Definition], initialContext: Context): Context = definitions.foldLeft(initialContext) { case (latestContext, definition) => {
    recordDefinition(definition, latestContext)
  }}
}
