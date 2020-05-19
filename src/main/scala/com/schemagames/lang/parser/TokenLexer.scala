package com.schemagames.lang.parser

import com.schemagames.lang.syntax.{IndentationType, NoIndent, Spaces, Tabs, Token}
import com.schemagames.lang.syntax.Tokens._

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens

object TokenLexer extends RegexParsers {
  def apply(code: String): Either[TokenLexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(TokenLexerError(next.pos, msg))
      case Success(result, _) => Right(result)
    }
  }

  // Skip most leading whitespace, but not \n newlines
  override def skipWhitespace: Boolean = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def tokens: Parser[List[Token]] = phrase(rep1(
    assign | delimiter | openBlock | closeBlock | openExpr | closeExpr | `def` | lambda | arrow |
      numLiteral | stringLiteral | identifier | newlineWithIndentation
  )) >> { rawTokens => {
    val tokenResult = for {
      indentedTokens <- processIndentation(rawTokens)
      delimitedTokens = indentationToDelimiting(indentedTokens)
      minimalDelimitedTokens = processDuplicateDelimiters(delimitedTokens)
    } yield minimalDelimitedTokens

    tokenResult match {
      case Left(errMsg) => err(errMsg)
      case Right(tokens) => success(tokens)
    }
  }}

  def identifier: Parser[Identifier] = positioned { "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) } }

  def stringLiteral: Parser[StringLiteral] = positioned { """"[^"]+"""".r ^^ (str => StringLiteral(str)) }

  def numLiteral: Parser[NumLiteral] = positioned { """[\d]+""".r ^^ (str => NumLiteral(str)) }

  def openBlock: Parser[OpenBlock] = positioned("{" ^^ (_ => OpenBlock()))
  def closeBlock: Parser[CloseBlock] = positioned("}" ^^ (_ => CloseBlock()))
  def openExpr: Parser[OpenExpr] = positioned("(" ^^ (_ => OpenExpr()))
  def closeExpr: Parser[CloseExpr] = positioned(")" ^^ (_ => CloseExpr()))
  def `def`: Parser[Def] = positioned("def" ^^ (_ => Def()))
  def assign: Parser[Assign] = positioned("=" ^^ (_ => Assign()))
  def delimiter: Parser[Delimit] = positioned(";" ^^ (_ => Delimit()))
  def lambda: Parser[Lambda] = positioned(("\\" | "λ") ^^ (_ => Lambda()))
  def arrow: Parser[Arrow] = positioned("->" ^^ (_ => Arrow()))

  def newlineWithIndentation: Parser[Indentation] = positioned(indentByMixedWhitespace)
  def indentByMixedWhitespace: Parser[Indentation] = "\n[ \t]*".r >> { str => // Mixed whitespace - this is a failure!
    val (numSpaces, numTabs) = str.drop(1).foldLeft((0,0)) { case ((curNumSpaces, curNumTabs), next) => next match {
      case ' '  => (curNumSpaces + 1, curNumTabs    )
      case '\t' => (curNumSpaces    , curNumTabs + 1)
      case _    => (curNumSpaces    , curNumTabs    )
    }}

    (numSpaces, numTabs) match {
      case (0, 0) => success(Indentation(0, NoIndent))
      case (0, t) => success(Indentation(t, Tabs))
      case (s, 0) => success(Indentation(s, Spaces))
      case (s, t) => failure(s"expected indentation with only spaces or only tabs, got $s spaces and $t tabs")
    }
  }

  // Convert Indentation of specific amounts into indents and outdents
  // If within a given "indentation block" we find mixed indentation types, the parser fails
  def processIndentation(
    tokens: List[Token],
    indentStack: List[(Int, IndentationType)] = List((0, NoIndent))
  ): Either[String, List[Token]] = tokens.headOption match {
    case None => Right(indentStack.drop(1).map(_ => Outdent()))
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
          (List(Delimit()), indentStack)
        }

        for {
          indentedTokens <- processIndentation(tokens.drop(1), newIndentStack)
        } yield {
          normalizeIndentation(newTokens, indentedTokens)
        }
      }
    }
    // Other tokens are just kept without any indentation tokens replacing them
    case Some(otherToken) => for {
      indentedTokens <- processIndentation(tokens.drop(1), indentStack)
    } yield otherToken :: indentedTokens
  }


  // While combining indentation tokens, remove adjacent Indents/Outdents
  // They "cancel" each other out, if no tokens exist in between them
  // We know newTokens are all of the same type, so removing from the front is fine
  @tailrec
  def normalizeIndentation(newTokens: List[Token], indentedTokens: List[Token]): List[Token] = {
    (newTokens, indentedTokens) match {
      case (Outdent() :: restNewTokens, Indent() :: restTokens) => normalizeIndentation(restNewTokens, Delimit() :: restTokens)
      case (Indent() :: restNewTokens, Outdent() :: restTokens) => normalizeIndentation(restNewTokens, Delimit() :: restTokens)
      case (otherNewTokens, indentedTokens) => otherNewTokens ++ indentedTokens
    }
  }

  def indentationToDelimiting(tokens: List[Token]): List[Token] = tokens.flatMap({
    case Indent() => None
    case Outdent() => Some(Delimit())
    case other => Some(other)
  })

  def processDuplicateDelimiters[A](tokens: List[Token]): List[Token] = {
    val (processedTokens, _) = tokens.foldLeft(List.empty[Token] -> false) {
      case ((processedTokens, true ), Delimit()) => (processedTokens             , true)
      case ((processedTokens, false), Delimit()) => (processedTokens :+ Delimit(), true)
      case ((processedTokens, _    ), other    ) => (processedTokens :+ other    , false)
    }

    processedTokens
  }
}
