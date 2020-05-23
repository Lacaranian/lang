package com.schemagames.lang.compiler

import com.schemagames.lang.Error

class PhaseKey[PhaseOutput](name: String) {
  override def toString: String = name.toString
}

trait Phase[In, Out, PhaseError <: Error] {
  val phaseKey: PhaseKey[Either[PhaseError, Out]] = new PhaseKey[Either[PhaseError, Out]](toString)

  def apply(input: In): Either[PhaseError, Out]

  def and[NextError <: Error, NextOut](nextPhase: Phase[Out, NextOut, NextError]): Phase[In, NextOut, Error] = {
    ComposedPhase[In, Out, NextOut, PhaseError, NextError](this, nextPhase)
  }
}

case class ComposedPhase[In, Out, NextOut, FstError <: Error, SndError <: Error](
  first: Phase[In, Out, FstError],
  next: Phase[Out, NextOut, SndError]
) extends Phase[In, NextOut, Error] {
  def apply(input: In): Either[Error, NextOut] = for {
    out <- first(input)
    nextOut <- next(out)
  } yield nextOut
}

case class DiscardInputPhase[In]() extends Phase[In, (), Error] {
  def apply(input: In): Either[Error, ()] = Right(())
}