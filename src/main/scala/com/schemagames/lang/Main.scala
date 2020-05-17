package com.schemagames.lang

import com.schemagames.lang.interpreter.Interpreter
import com.schemagames.lang.parser.{ASTParser, TokenLexer}
import com.schemagames.lang.syntax.Token

object test {
  val n1 =
    """def testProgram = {
      |  def floop = 123
      |  def bloop = floop
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
      _ = println(tokens)
      ast <- ASTParser(tokens)
      _ = println(ast)
      result <- Interpreter(ast)
    } yield result

    println(result)
  }
}
