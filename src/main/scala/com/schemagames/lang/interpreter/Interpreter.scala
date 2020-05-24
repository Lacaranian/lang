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
    val Definition(vt : VariableTerm, expr) = definition

    val term = evaluateExpression(expr, context)

    context.copy(symbolTable = context.symbolTable + (vt.identifier -> term))
  }

  @tailrec
  def evaluateExpression(expr: Expression, context: Context): Term = expr match {
    case term: Term => evaluateTerm(term, context)
    case be: BlockExpression => {
      // The context and changes to it in this block, stay in this block (scoping)
      val bContext = blockContext(be.definitions, context)

      evaluateExpression(be.finalExpr, bContext)
    }
  }

  def evaluateTerm(term: Term, context: Context): Term = term match {
    case c: Constant => c
    // Perhaps substitute a variable's value in this context for its term
    case v: VariableTerm => context.symbolTable.getOrElse(v.identifier, v)
    case l: Abstraction => l
    case app: Application => {
      // Strict style, evaluate both the halves before applying them
      val applyingTerm = evaluateExpression(app.applyingTerm, context)
      val appliedTerm = evaluateExpression(app.appliedTerm, context)

      applyingTerm match {
        case abs: Abstraction => {
          val contextWithLambdaParamFree = context.copy(symbolTable = context.symbolTable - abs.variable.identifier)
          val substitutedLambdaBody = substituteFreeOccurrencesInExpression(abs.variable, abs.expression, appliedTerm, contextWithLambdaParamFree)

          // β-reduction
          evaluateExpression(substitutedLambdaBody, context)
        }
        case at => Application(at, appliedTerm)
      }
    }
  }

  def substituteFreeOccurrencesInTerm(
    variable: VariableTerm,
    term: Term,
    replaceWith: Term,
    context: Context
  ): Term = term match {
    case c: Constant => c
    case vt: VariableTerm if vt == variable => replaceWith
    case v: VariableTerm => v
    case abs: Abstraction if abs.variable != variable => {
      val innerLambdaContext = context.copy(symbolTable = context.symbolTable - abs.variable.identifier)

      // Ensure paramVariable is not in the free variables of replaceWith (capture avoiding substitution)
      val replaceFreeVars = freeVariablesOfTerm(replaceWith, context)
      val safeReplaceWith = if(replaceFreeVars(abs.variable)) {
        val safeParamVar = VariableTerm(abs.variable.identifier + "'") // Just prime it (is this enough?)
        substituteFreeOccurrencesInTerm(abs.variable, replaceWith, safeParamVar, context)
      } else {
        replaceWith
      }

      val substitutedBody = substituteFreeOccurrencesInExpression(variable, abs.expression, safeReplaceWith, innerLambdaContext)

      Abstraction(abs.variable, substitutedBody)
    }
    case l: Abstraction => l // variable is not "free" in the lambda
    case app: Application => Application(
      substituteFreeOccurrencesInExpression(variable, app.applyingTerm, replaceWith, context),
      substituteFreeOccurrencesInExpression(variable, app.appliedTerm, replaceWith, context)
    )
  }

  def substituteFreeOccurrencesInExpression(
    variable: VariableTerm,
    expression: Expression,
    replaceWith: Term,
    context: Context
  ): Expression = expression match {
    case term: Term => substituteFreeOccurrencesInTerm(variable, term, replaceWith, context)
    case blockExpr: BlockExpression => {
      val (substitutedDefinitions, newContext) = blockExpr.definitions.foldLeft(List.empty[Definition] -> context) {
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
      val substitutedFinalTerm = substituteFreeOccurrencesInExpression(variable, blockExpr.finalExpr, replaceWith, newContext)

      BlockExpression(substitutedDefinitions, substitutedFinalTerm)
    }
  }

  def freeVariablesOfTerm(term: Term, context: Context): Set[VariableTerm] = (term match {
    case _: Constant         => Set.empty[VariableTerm]
    // Variables are free as long as they aren't bound in the context
    case v: VariableTerm     => Set(v)
    // Abstractions have all free variables of the body, minus the parameter (which binds it)
    case abs: Abstraction    => freeVariablesOfExpression(abs.expression, context) - abs.variable
    // Applications have all the free variables of both terms
    case app: Application    => freeVariablesOfExpression(app.applyingTerm, context) ++ freeVariablesOfExpression(app.appliedTerm, context)
  }).filterNot(v =>
    // Anything bound in the context is not free
    context.symbolTable.contains(v.identifier)
  )


  def freeVariablesOfExpression(expression: Expression, context: Context): Set[VariableTerm] = expression match {
    case term: Term                 => freeVariablesOfTerm(term, context)
    case blockExpr: BlockExpression => {
      // In BlockExpressions, variables appear free until they are bound in a definition
      // From the outside, we only consider a variable fully free if it is never bound this way
      val bContext: Context = blockContext(blockExpr.definitions, context)

      freeVariablesOfExpression(blockExpr.finalExpr, bContext)
    }
  }

  def blockContext(definitions: List[Definition], initialContext: Context): Context = definitions.foldLeft(initialContext) { case (latestContext, definition) => {
    recordDefinition(definition, latestContext)
  }}
}
