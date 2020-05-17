package com.schemagames.lang.syntax

import scala.util.parsing.input.Positional

sealed trait Token extends Positional

object Tokens {
  case class Identifier(name: String) extends Token
  case class StringLiteral(str: String) extends Token
  case class NumLiteral(str: String) extends Token

  case class OpenBlock()   extends Token // {
  case class CloseBlock()  extends Token // }
  case class Def()         extends Token // def
  case class Assign()      extends Token // =
  case class Newline()     extends Token // \n

  case class Delimit()     extends Token // ; \n

  case class NewlineWithIndentation(n: Newline, i: Indentation) extends Token
  case class Indentation(num: Int, indentationType: IndentationType) extends Token // \n plus some number of spaces/tabs...
  case class Indent()         extends Token // ... greater then the previous line's
  case class Outdent()        extends Token // ... less than the previous line's

}

// Mixing indentation types is a lexer error
sealed trait IndentationType
case object Tabs   extends IndentationType
case object Spaces extends IndentationType