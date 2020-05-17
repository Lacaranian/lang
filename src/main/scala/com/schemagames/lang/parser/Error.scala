package com.schemagames.lang.parser

import scala.util.parsing.input.Position

sealed trait Error
case class TokenLexerError(location: Position, msg: String) extends Error
case class ASTParserError(location: Position, msg: String) extends Error
case class InterpreterError(msg: String) extends Error