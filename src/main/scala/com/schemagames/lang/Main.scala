package com.schemagames.lang

import com.schemagames.lang.interpreter.Interpreter
import com.schemagames.lang.parser.{ASTParser, LexerToParser, TokenLexer}

object test {
  val n1 =
    """def testProgram = {
      |  def floop = 123
      |  def bloop = floop; def ploop = snoop;
      |  def noop = "nope"
      |
      |  noop
      |}
      |
      |def main = testProgram
      |""".stripMargin
}

object Main {
  def main(args: Array[String]): Unit = {
    val result = for {
      tokens <- TokenLexer(test.n1)
      syntaxReadyTokens = LexerToParser(tokens)
      _ = println(syntaxReadyTokens)
      ast <- ASTParser(syntaxReadyTokens)
      _ = println(ast)
      result <- Interpreter(ast)
    } yield result

    println(result)
  }
}
