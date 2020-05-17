package com.schemagames.lang.syntax

import scala.util.parsing.input.Positional

sealed trait Token extends Positional

object Tokens {
  case class Identifier(name: String) extends Token
  case class StringLiteral(str: String) extends Token
  case class NumLiteral(str: String) extends Token

  case class OpenBlock()  extends Token // {
  case class CloseBlock() extends Token // }
  case class Def()        extends Token // def
  case class Assign()     extends Token // =
  //case class Newline()    extends Token // \n
}
