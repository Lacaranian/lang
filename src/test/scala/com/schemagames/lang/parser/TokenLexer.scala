package com.schemagames.lang.parser

import com.schemagames.lang.syntax.Tokens.{Indent, NumLiteral, OpenBlock, Outdent}
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class TokenLexerSpec extends FlatSpec with Matchers {

  "The TokenLexer" should "be able to turn a valid input program into tokens" in {
    val program =
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

    val results = TokenLexer(program)
    results shouldBe a [Right[_, _]]
  }

  it should "recognize indentation and outdentation after newlines" in {
    val program =
      """def program = {
        |  def thing = 1
        |}""".stripMargin
    val results = TokenLexer(program)
    results shouldBe a [Right[_, _]]

    val Right(tokens) = results
    val afterFirstNewline = tokens.dropWhile(_ != OpenBlock()).drop(1)

    afterFirstNewline.headOption should be (Some(Indent()))

    val afterSecondNewline = afterFirstNewline.dropWhile(_ != NumLiteral("1")).drop(1)

    afterSecondNewline.headOption should be (Some(Outdent()))
  }
}
