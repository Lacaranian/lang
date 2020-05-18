package com.schemagames.lang.parser

import com.schemagames.lang.TestPrograms
import com.schemagames.lang.syntax.SyntaxTree.{Application, Definition, TermExpression, Variable, VariableTerm}
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class ASTParserSpec extends FlatSpec with Matchers {
  "The ASTParser" should "be able to turn a valid token stream into an an abstract syntax tree" in {
    val Right(tokens) = TokenLexer(TestPrograms.generalTestProgram)
    val results = ASTParser(tokens)

    results shouldBe a [Right[_, _]]
  }

  it should "default to left-associativity of applied terms" in {
    val Right(tokens) = TokenLexer(TestPrograms.applicationAssociativityTest)
    val results = ASTParser(tokens)

    results shouldBe a [Right[_, _]]

    val Right(ast) = results

    ast.head should be (Definition(Variable("test"), TermExpression(
      Application(
        Application(
          Application(
            VariableTerm(Variable("a")),
            VariableTerm(Variable("b"))
          ),
          VariableTerm(Variable("c"))
        ),
        VariableTerm(Variable("d"))
      )
    )))
  }
}
