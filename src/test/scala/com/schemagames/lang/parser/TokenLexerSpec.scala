package com.schemagames.lang.parser

import com.schemagames.lang.syntax.Tokens.{Def, Delimit, Identifier, Indent, NumLiteral, OpenBlock, Outdent}
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class TokenLexerSpec extends FlatSpec with Matchers {

  "The TokenLexer" should "be able to turn a valid input program into tokens" in {
    val program =
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

    val results = TokenLexer(program)
    results shouldBe a [Right[_, _]]
  }

  it should "recognize indentation and outdentation after newlines" in {
    val program =
      """def program = {
        |  def thing = 1
        |
        |  thing
        |}""".stripMargin
    val results = TokenLexer(program)
    results shouldBe a [Right[_, _]]

    val Right(tokens) = results
    val afterFirstNewline = tokens.dropWhile(_ != OpenBlock()).drop(1)

    afterFirstNewline.headOption should be (Some(Indent()))

    val atSecondNewline = afterFirstNewline.dropWhile(_ != NumLiteral("1")).drop(1)

    atSecondNewline.headOption should be (Some(Delimit()))

    val afterThingNewline = atSecondNewline.dropWhile(_ != Identifier("thing")).drop(1)

    afterThingNewline.headOption should be (Some(Outdent()))
  }

  it should "sustain indentation across empty lines" in {
    val program =
      """def program = {
        |  def thing = 1
        |
        |  def thing2 = 2
        |
        |
        |
        |  thing2
        |}""".stripMargin
    val results = TokenLexer(program)
    results shouldBe a [Right[_, _]]

    val Right(tokens) = results
    val afterFirstNewline = tokens.dropWhile(_ != OpenBlock()).drop(1)

    afterFirstNewline.headOption should be (Some(Indent()))

    val afterSecondNewline = afterFirstNewline.dropWhile(_ != NumLiteral("1")).drop(1)

    afterSecondNewline.headOption should be (Some(Delimit()))

    val afterThing2Line = afterSecondNewline.dropWhile(_ != NumLiteral("2")).drop(1)

    afterThing2Line.headOption should be (Some(Delimit()))
    afterThing2Line.drop(1).headOption should be (Some(Identifier("thing2")))

    val afterFinalExpressionLine = afterThing2Line.dropWhile(_ != Identifier("thing2")).drop(1)
    afterFinalExpressionLine.headOption should be (Some(Outdent()))
  }

  it should "prevent mixing of tabs and spaces in indentation" in {
    val program = "def testProgram = {\n  \t  def something = 123\n\t  something\n}"
    val results = TokenLexer(program)
    results shouldBe a [Left[_, _]]
  }
}
