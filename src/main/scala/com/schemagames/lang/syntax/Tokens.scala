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
  case class Delimit()     extends Token // ; or newlines with sustained indentation (outdents are also semantically delimiters)

  case class Indentation(num: Int, indentationType: IndentationType) extends Token // \n plus some number of spaces/tabs...
  case class Indent()         extends Token // ... greater then the previous significant line's
  case class Outdent()        extends Token // ... less than the previous significant line's
}

// Mixing indentation types is a lexer error
sealed trait IndentationType
case object NoIndent extends IndentationType
case object Tabs     extends IndentationType
case object Spaces   extends IndentationType