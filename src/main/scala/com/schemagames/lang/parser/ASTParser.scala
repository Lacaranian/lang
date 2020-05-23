package com.schemagames.lang.parser

import com.schemagames.lang.ASTParserError
import com.schemagames.lang.compiler.Phase
import com.schemagames.lang.syntax.{Token, Tokens, UntypedAST}
import com.schemagames.lang.syntax.Tokens._
import com.schemagames.lang.syntax.UntypedAST._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}

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


case object ASTParser extends Parsers with Phase[List[Token], List[UntypedAST], ASTParserError] {
  def apply(tokens: List[Token]): Either[ASTParserError, List[UntypedAST]] = {
    val reader = new TokenReader(tokens)

    ast(reader) match {
      case NoSuccess(msg, next) => Left(ASTParserError(next.pos, msg))
      case Success(result, _) => Right(result)
    }
  }

  def ast: Parser[List[UntypedAST]] = phrase(rep1(definition))

  override type Elem = Token

  def definition: Parser[Definition] = positioned {
    (Def() ~ variable ~ Assign() ~ expressions ~ (Delimit() | eoi)) ^^ {
      case _ ~ variable ~ _ ~ expr ~ _ => Definition(variable, expr)
    }
  }
  def variable: Parser[Variable] = positioned(identifier ^^ (id => Variable(id.name)))
  private def identifier: Parser[Tokens.Identifier] = positioned(accept("identifier", { case id @ Identifier(_) => id }))

  def expressions: Parser[Expression] = positioned(rep1(blockExpression | groupedExpression | term) >> {
    case Nil             => failure("Got no expressions from a successful rep1 parser")
    case single :: Nil   => success(single)
    case firstOf :: many => success(leftAssociateExpressionsInApplications(firstOf, many))
  })
  def blockExpression: Parser[BlockExpression] = positioned{
    OpenBlock() ~ rep(definition) ~ expressions ~ Delimit() ~ CloseBlock() ^^ {
      case _ ~ defs ~ expr ~ _ ~ _ => BlockExpression(defs, expr)
    }
  }
  def groupedExpression: Parser[Expression] = positioned(OpenExpr() ~ expressions ~ CloseExpr() ^^ {
    case _ ~ expr ~ _ => expr
  })
  def term: Parser[Term] = positioned(lambdaTerm | variableTerm | literal)

  def lambdaTerm: Parser[Abstraction] = positioned(Lambda() ~ variable ~ Arrow() ~ expressions ^^ { case _ ~ param ~ _ ~ expr => Abstraction(param, expr) })
  def variableTerm: Parser[VariableTerm] = positioned(variable ^^ (variable => VariableTerm(variable)))

  def literal: Parser[Constant] = positioned(stringLiteral | numLiteral)
  def stringLiteral: Parser[StringConstant] = positioned(accept("string constant", { case StringLiteral(str) => StringConstant(str) }))
  def numLiteral: Parser[NumberConstant] = positioned(accept("number constant", { case NumLiteral(str) => NumberConstant(str.toInt) }))

  def eoi: Parser[Delimit] = new Parser[Delimit]{
    def apply(in: Input): ParseResult[Delimit] = {
      if(in.atEnd) Success(Delimit(), in) else Failure("expected end of input", in)
    }
  }

  def leftAssociateExpressionsInApplications(firstExpr: Expression, restExprs: List[Expression]): Expression = restExprs match {
    case Nil        => firstExpr
    case x ::  Nil  => Application(firstExpr, x)
    case x ::  rest => leftAssociateExpressionsInApplications(Application(firstExpr, x), rest)
  }
}
