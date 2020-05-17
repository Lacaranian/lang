package com.schemagames.lang.parser

import com.schemagames.lang.syntax.{IndentationType, NoIndent, Spaces, Tabs, Token}
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

  // Skip most leading whitespace, but not \n newlines
  // Also, don't skip leading whitespace when lexing the first indentation on a line
  override def skipWhitespace: Boolean = skipWhiteSpaceOverride.getOrElse(true)
  var skipWhiteSpaceOverride: Option[Boolean] = None
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def tokens: Parser[List[Token]] = phrase(rep1(
    assign | delimiter | openBlock | closeBlock | `def` | numLiteral | stringLiteral | identifier | newlineWithIndentation
  )) >> { rawTokens => {
    processIndentation(rawTokens) match {
      case Left(errMsg) => err(errMsg)
      case Right(tokens) => success(tokens)
    }
  }}

  def identifier: Parser[Identifier] = positioned { "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) } }

  def stringLiteral: Parser[StringLiteral] = positioned { """"[^"]+"""".r ^^ (str => StringLiteral(str)) }

  def numLiteral: Parser[NumLiteral] = positioned { """[\d]+""".r ^^ (str => NumLiteral(str)) }

  def openBlock: Parser[OpenBlock] = positioned("{" ^^ (_ => OpenBlock()))
  def closeBlock: Parser[CloseBlock] = positioned("}" ^^ (_ => CloseBlock()))
  def `def`: Parser[Def] = positioned("def" ^^ (_ => Def()))
  def assign: Parser[Assign] = positioned("=" ^^ (_ => Assign()))
  def delimiter: Parser[Delimit] = positioned(";" ^^ (_ => Delimit()))

  def newlineWithIndentation: Parser[Indentation] = positioned(indentBySpaces | indentByTabs | failIndentByMixedWhitespace)
  def indentBySpaces: Parser[Indentation] = indentByChar(Spaces, " ")
  def indentByTabs: Parser[Indentation] = indentByChar(Tabs, "\t")
  def indentByChar(indentType: IndentationType, regexChar: String): Parser[Indentation] = s"\n[$regexChar]*".r ^^ (str => {
    val numChars = str.length()
    Indentation(str.length - 1, if(numChars == 0) NoIndent else indentType)
  })
  // Always fails, as this is explicitly prohibited
  def failIndentByMixedWhitespace: Parser[Nothing] = ("\n[ \t]*".r ^^ { str => // Mixed whitespace - this is a failure!
    val (numSpaces, numTabs) = str.foldLeft((0,0)) { case ((curNumSpaces, curNumTabs), next) => next match {
      case ' '  => (curNumSpaces + 1, curNumTabs    )
      case '\t' => (curNumSpaces    , curNumTabs + 1)
      case _    => (curNumSpaces    , curNumTabs    )
    }}

    (numSpaces, numTabs)
  }) >> { case (numSpaces, numTabs) =>  failure(
    s"expected indentation with only spaces or only tabs, got $numSpaces spaces and $numTabs tabs"
  ) }

  // Convert Indentation of specific amounts into indents and outdents
  // If within a given "indentation block" we find mixed indentation types, the parser fails
  // TODO: Avoid ..., Outdent(), Indent(), ...
  def processIndentation(
    tokens: List[Token],
    indentStack: List[(Int, IndentationType)] = List((0, NoIndent))
  ): Either[String, List[Token]] = tokens.headOption match {
    case None => Right(indentStack.map(_ => Outdent()))
    case Some(Indentation(amt, indentType)) => indentStack.head match {
      // Mixed indentation error
      case (_, prevIndentType) if {
        prevIndentType != indentType && prevIndentType != NoIndent && indentType != NoIndent
      } => Left(
        "Line found with indentation characters mismatching those from the previous indentation!"
      )
      // Good indentation, compare to the previous amount to determine the kind
      case (prevAmt, _) => {
        val (newTokens, newIndentStack) = if(amt > prevAmt) {
          // Indenting further out
          (List(Indent()), (amt, indentType) :: indentStack)
        } else if(amt < prevAmt) {
          // Outdenting back in
          val (dropped, kept) = indentStack.partition(_._1 > amt)
          (dropped.map(_ => Outdent()), kept)
        } else {
          // Staying at the same level of indentation
          (Nil, indentStack)
        }

        for {
          indentedTokens <- processIndentation(tokens.drop(1), newIndentStack)
        } yield newTokens ++ indentedTokens
      }
    }
    // Other tokens are just kept without any indentation tokens replacing them
    case Some(otherToken) => for {
      indentedTokens <- processIndentation(tokens.drop(1), indentStack)
    } yield otherToken :: indentedTokens
  }
}
