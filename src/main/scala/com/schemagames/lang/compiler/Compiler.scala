package com.schemagames.lang.compiler

import com.schemagames.lang.interpreter.Interpreter
import com.schemagames.lang.parser.{ASTParser, TokenLexer}
import com.schemagames.lang.Error

object DefaultCompiler extends Compiler(TokenLexer and ASTParser and Interpreter and DiscardInputPhase())

case class Compiler(
  allPhases: Phase[String, (), Error],
  context: CompilerContext = CompilerContext()
) {
  type Input = String

  def apply(input: Input): Compiler = {
    // Start the run
    val thisRunID = context.curRunID + 1
    val runContext = context.copy(
      curRunID = thisRunID,
      pastRuns = context.pastRuns + (thisRunID -> RunInfo(input))
    )

    // Run the compiler (recording results as each phase as we go)
    val (compilationResult: CompilerContext, _) = runPhase(runContext, allPhases, input)

    // Incremement the run counter for future runs
    copy(context = compilationResult)
  }

  def runPhase[In, Out, PhaseError <: Error](
    compilerContext: CompilerContext,
    phase: Phase[In, Out, PhaseError],
    input: In
  ): (CompilerContext, Either[PhaseError, Out]) = phase match {
    case ComposedPhase(firstPhase, nextPhase) => {
      val (firstResultContext, firstResult) = runPhase(compilerContext, firstPhase, input)
      firstResult match {
        case Left(error)  => {
          // The last phase failed, just finish with the current context
          (compilerContext, Left(error))
        }
        case Right(firstOutput) => runPhase(firstResultContext, nextPhase, firstOutput)
      }
    }
    case basicPhase => {
      val result: Either[PhaseError, Out] = basicPhase.apply(input)

      // Record the result in the context for posterity
      val updatedCompilerContext = compilerContext.recordPhaseResult(phase, result)

      updatedCompilerContext -> result
    }
  }
}

case class CompilerContext(
  curRunID: Int = 0,
  // Inspired by Dotty, maintain build information across multiple runs/phases
  // If the source doesn't change, and the compiler didn't change, neither need the output - reduce, reuse, recycle
  pastRuns: Map[Int, RunInfo] = Map()
) {
  def recordPhaseResult[Out, PhaseError <: Error](
    phase: Phase[_, Out, PhaseError],
    result: Either[PhaseError, Out]
  ): CompilerContext = {
    // We only record a result if the run is tracked by the context
    val maybeNewRunInfo = pastRuns.get(curRunID).map(runInfo => {
      val newPhaseRunInfo: Map[PhaseKey[_], Either[Error, Any]] = runInfo.phaseResults + (phase.phaseKey -> result)

      curRunID -> runInfo.copy(phaseResults = newPhaseRunInfo)
    })
    val newAllRunsInfo: Map[Int, RunInfo] = maybeNewRunInfo match {
      case Some(newRunInfo) => pastRuns + newRunInfo
      case None => pastRuns
    }

    copy(pastRuns = newAllRunsInfo)
  }
}

case class RunInfo(
  startingInput: String,
  phaseResults: Map[PhaseKey[_], Either[Error, Any]] = Map() /* Progressive output at each phase */
)