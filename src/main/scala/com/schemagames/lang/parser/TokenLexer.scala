package com.schemagames.lang.parser

import com.schemagames.lang.syntax.{IndentationType, Spaces, Tabs, Token}
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
    assign | openBlock | closeBlock | `def` | numLiteral | stringLiteral | identifier | newlineWithIndentation
  )) >> { rawTokens => {
    val splitTokens = splitOutNewlinesWithIndentation(rawTokens)
    processIndentation(splitTokens) match {
      case Left(errMsg) => err(errMsg)
      case Right(tokens) => success(tokens)
    }
  }}

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

  def newlineWithIndentation: Parser[NewlineWithIndentation] = newline ~ indentation ^^ {
    case newline ~ indent => NewlineWithIndentation(newline, indent)
  }
  def newline: Parser[Newline] = positioned("\n" ^^ (_ => Newline()))
  def indentation: Parser[Indentation] = withLeadingWhitespace(
    positioned(indentationBySpaces | indentationByTabs | indentationByMixedWhitespace)
  )
  def indentationBySpaces: Parser[Indentation] = withLeadingWhitespace("[ ]*".r) ^^ { str => // Just spaces
    Indentation(str.length, Spaces)
  }
  def indentationByTabs: Parser[Indentation] = withLeadingWhitespace("[\t]*".r) ^^ { str => // Just tabs
    Indentation(str.length, Tabs)
  }
  // Always fails, as this is explicitly prohibited
  def indentationByMixedWhitespace: Parser[Nothing] = (withLeadingWhitespace("[ \t]*".r) ^^ { str => // Mixed whitespace - this is a failure!
    // TODO: Single pass count
    val numSpaces = str.count(_ == ' ')
    val numTabs = str.count(_ == '\t')

    (numSpaces, numTabs)
  }) >> { case (numSpaces, numTabs) =>  failure(
    s"expected indentation with only spaces or only tabs, got $numSpaces spaces and $numTabs tabs"
  ) }

  def delimit: Parser[Delimit] = positioned("[;\n]".r ^^ (_ => Delimit()))

  def splitOutNewlinesWithIndentation(tokens: List[Token]): List[Token] = tokens.flatMap{
    case NewlineWithIndentation(n, i) => List(n, i)
    case other => List(other)
  }

  // Convert Indentation of specific amounts into indents and outdents
  // If within a given "indentation block" we find mixed indentation types, the parser fails
  // TODO: Avoid ..., Outdent(), Indent(), ...
  def processIndentation(
    tokens: List[Token],
    indentStack: List[(Int, IndentationType)] = Nil //(amt > 0, indent type)
  ): Either[String, List[Token]] = {
    tokens.headOption match {
      case None => Right(indentStack.map(_ => Outdent()))
      case Some(Indentation(amt, indentType)) => indentStack.headOption match {
        // Mixed indentation
        case Some((_, prevIndentType)) if prevIndentType != indentType => Left(
          "Line found with indentation characters mismatching those from the previous indentation!"
        )
        // Indenting further out
        case Some((prevAmt, _)) if amt > prevAmt => for {
          indentedTokens <- processIndentation(tokens.drop(1), (amt, indentType) :: indentStack)
        } yield Indent() :: indentedTokens
        // Outdenting back in
        case Some((prevAmt, _)) if amt < prevAmt => {
          val (dropped, kept) = indentStack.partition(_._1 > amt)
          for {
            indentedTokens <- processIndentation(tokens.drop(1), kept)
          } yield dropped.map(_ => Outdent()) ++ indentedTokens
        }
        // Staying at current indentation
        case Some((prevAmt, _)) if amt == prevAmt => for {
          indentedTokens <- processIndentation(tokens.drop(1), indentStack)
        } yield indentedTokens
        // Special case of indenting from no previous indentation
        case None if amt > 0 => for {
          indentedTokens <- processIndentation(tokens.drop(1), (amt, indentType) :: indentStack)
        } yield Indent() :: indentedTokens
        // Staying at no indentation
        case None => for {
          indentedTokens <- processIndentation(tokens.drop(1), indentStack)
        } yield indentedTokens
      }
      // Other tokens are just kept without any indentation tokens replacing them
      case Some(otherToken) => for {
        indentedTokens <- processIndentation(tokens.drop(1), indentStack)
      } yield otherToken :: indentedTokens
    }
  }

  def withLeadingWhitespace[A](p: Parser[A]): Parser[A] = { (in: Input) => {
    val origSkipWhiteSpaceOverride = skipWhiteSpaceOverride
    skipWhiteSpaceOverride = Some(false)
    val result = p.apply(in)
    skipWhiteSpaceOverride = origSkipWhiteSpaceOverride
    result
  }}
}

// Cribbed from scala.util.parsing.combinator, as it was private
private class SubSequence(s: CharSequence, start: Int, val length: Int) extends CharSequence {
  def this(s: CharSequence, start: Int) = this(s, start, s.length - start)

  def charAt(i: Int) =
    if (i >= 0 && i < length) s.charAt(start + i) else throw new IndexOutOfBoundsException(s"index: $i, length: $length")

  def subSequence(_start: Int, _end: Int) = {
    if (_start < 0 || _end < 0 || _end > length || _start > _end)
      throw new IndexOutOfBoundsException(s"start: ${_start}, end: ${_end}, length: $length")

    new SubSequence(s, start + _start, _end - _start)
  }

  override def toString = s.subSequence(start, start + length).toString
}
