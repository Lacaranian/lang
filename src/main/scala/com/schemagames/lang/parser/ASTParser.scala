package com.schemagames.lang.parser

import com.schemagames.lang.syntax.{SyntaxTree, Token, Tokens}
import com.schemagames.lang.syntax.Tokens._
import com.schemagames.lang.syntax.SyntaxTree._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, OffsetPosition, Position, Reader}

case class TokenStreamPosition(tokens: Seq[Token]) extends Position {
  override def line: Int = tokens.headOption.map(_.pos.line).getOrElse(-1)
  override def column: Int = tokens.headOption.map(_.pos.column).getOrElse(-1)

  override protected def lineContents: String = tokens.takeWhile(_.pos.line == line).mkString(", ")
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = TokenStreamPosition(tokens)
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}


object ASTParser extends Parsers {
  def apply(tokens: List[Token]): Either[ASTParserError, List[SyntaxTree]] = {
    val reader = new TokenReader(tokens)

    ast(reader) match {
      case NoSuccess(msg, next) => Left(ASTParserError(next.pos, msg))
      case Success(result, _) => Right(result)
    }
  }

  def ast: Parser[List[SyntaxTree]] = phrase(rep1(definition)) ^^ { rawAST => rawAST }

  override type Elem = Token

  def definition: Parser[Definition] = positioned {
    (Def() ~ variable ~ Assign() ~ expression) ^^ {
      case _ ~ variable ~ _ ~ expr => Definition(variable, expr)
    }
  }
  def variable: Parser[Variable] = positioned(identifier ^^ (id => Variable(id.name)))
  private def identifier: Parser[Tokens.Identifier] = positioned(accept("identifier", { case id @ Identifier(_) => id }))

  def expression: Parser[Expression] = positioned(blockExpression | termExpression)
  def blockExpression: Parser[BlockExpression] = positioned{
    OpenBlock() ~ rep(definition) ~ expression ~ CloseBlock() ^^ {
      case _ ~ defs ~ expr ~ _ => BlockExpression(defs, expr)
    }
  }
  def termExpression: Parser[TermExpression] = positioned(term ~ Delimit() ^^ {
    case term ~ _ => TermExpression(term)
  })

  def term: Parser[Term] = positioned(constant | variableTerm)
  def variableTerm: Parser[VariableTerm] = positioned(variable ^^ (variable => VariableTerm(variable)))

  def constant: Parser[Constant] = positioned(stringConstant | numConstant)
  def stringConstant: Parser[StringConstant] = positioned(accept("string constant", { case StringLiteral(str) => StringConstant(str) }))
  def numConstant: Parser[NumberConstant] = positioned(accept("number constant", { case NumLiteral(str) => NumberConstant(str.toInt) }))
}
