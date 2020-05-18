package com.schemagames.lang

import com.schemagames.lang.interpreter.Interpreter
import com.schemagames.lang.parser.{ASTParser, TokenLexer}

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
  val n2 = "def test = a b c d"
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
