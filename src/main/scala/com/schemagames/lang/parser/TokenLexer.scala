package com.schemagames.lang.parser

import com.schemagames.lang.syntax.Token
import com.schemagames.lang.syntax.Tokens._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object TokenLexer extends RegexParsers {
  def apply(code: String): Either[TokenLexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(TokenLexerError(next.pos, msg))
      case Success(result, _) => Right(result)
    }
  }

  // Skip most whitespace, but not \n newlines
  //override def skipWhitespace: Boolean = true
  //override val whiteSpace: Regex = "[ \t\r\f]+".r

  def tokens: Parser[List[Token]] = phrase(rep1(
    assign | openBlock | closeBlock | `def` | numLiteral | stringLiteral | identifier
  )) ^^ { rawTokens => rawTokens }

  def identifier: Parser[Identifier] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) }
  }

  def stringLiteral: Parser[StringLiteral] = positioned {
    """"[^"]+"""".r ^^ { str => StringLiteral(str) }
  }

  def numLiteral: Parser[NumLiteral] = positioned {
    """[\d]+""".r ^^ { str => NumLiteral(str) }
  }

  def openBlock: Parser[OpenBlock] = positioned("{" ^^ (_ => OpenBlock()))
  def closeBlock: Parser[CloseBlock] = positioned("}" ^^ (_ => CloseBlock()))
  def `def`: Parser[Def] = positioned("def" ^^ (_ => Def()))
  def assign: Parser[Assign] = positioned("=" ^^ (_ => Assign()))
  //def newline: Parser[Newline] = positioned("\n".r ^^ (_ => Newline()))
}
