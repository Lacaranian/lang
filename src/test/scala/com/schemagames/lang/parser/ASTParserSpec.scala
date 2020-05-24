package com.schemagames.lang.parser

import com.schemagames.lang.TestPrograms
import com.schemagames.lang.syntax.UntypedAST.{Application, Definition, VariableTerm}
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ASTParserSpec extends AnyFlatSpec with Matchers {
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

    ast.head should be (Definition(VariableTerm("test"),
      Application(
        Application(
          Application(
            VariableTerm("a"),
            VariableTerm("b")
          ),
          VariableTerm("c")
        ),
        VariableTerm("d")
      )
    ))
  }

  it should "parse abstraction and application terms" in {
    val Right(tokens) = TokenLexer(TestPrograms.abstractionAndApplicationTest)
    val results = ASTParser(tokens)

    results shouldBe a [Right[_, _]]
  }

  it should "parse annotated variables" in {
    val Right(tokens) = TokenLexer(TestPrograms.annotatedWithTypesTest)
    val results = ASTParser(tokens)

    results shouldBe a [Right[_, _]]
  }
}
