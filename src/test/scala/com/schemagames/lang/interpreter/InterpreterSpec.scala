package com.schemagames.lang.interpreter

import com.schemagames.lang.parser.{ASTParser, TokenLexer}
import com.schemagames.lang.{TestPrograms, test}
import com.schemagames.lang.syntax.UntypedAST.StringConstant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterSpec extends AnyFlatSpec with Matchers {
  def interpret(inputProgram: String) = {
    for {
      tokens <- TokenLexer(inputProgram)
      ast <- ASTParser(tokens)
      result <- Interpreter(ast)
    } yield result
  }

  "The Interpreter" should "be able to interpret a program" in {
    val result = interpret(TestPrograms.generalTestProgram)

    result shouldBe a [Right[_,_]]
    val Right(returnValue) = result

    returnValue should be (StringConstant("nope"))
  }

  it should "be able to interpret a progam using lambda combinators" in {
    val result = interpret(TestPrograms.abstractionAndApplicationTest)

    result shouldBe a [Right[_,_]]
    val Right(returnValue) = result

    returnValue should be (StringConstant("false"))
  }
}
