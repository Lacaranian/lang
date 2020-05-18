package com.schemagames.lang.parser

import com.schemagames.lang.TestPrograms
import com.schemagames.lang.syntax.Tokens._
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class TokenLexerSpec extends FlatSpec with Matchers {

  "The TokenLexer" should "be able to turn a valid input program into tokens" in {
    val results = TokenLexer(TestPrograms.generalTestProgram)
    results shouldBe a [Right[_, _]]
  }

  it should "recognize indentation and outdentation after newlines" in {
    val results = TokenLexer(TestPrograms.simpleWhitespace)
    results shouldBe a [Right[_, _]]

    val Right(tokens) = results
    val afterFirstNewline = tokens.dropWhile(_ != OpenBlock()).drop(1)

    afterFirstNewline.headOption should be (Some(Def()))

    val atSecondNewline = afterFirstNewline.dropWhile(_ != NumLiteral("1")).drop(1)

    atSecondNewline.headOption should be (Some(Delimit()))

    val afterThingNewline = atSecondNewline.dropWhile(_ != Identifier("thing")).drop(1)

    afterThingNewline.headOption should be (Some(Delimit()))
  }

  it should "sustain indentation across empty lines" in {
    val results = TokenLexer(TestPrograms.longerWhitespace)
    results shouldBe a [Right[_, _]]

    val Right(tokens) = results
    val afterFirstNewline = tokens.dropWhile(_ != OpenBlock()).drop(1)

    afterFirstNewline.headOption should be (Some(Def()))

    val afterSecondNewline = afterFirstNewline.dropWhile(_ != NumLiteral("1")).drop(1)

    afterSecondNewline.headOption should be (Some(Delimit()))

    val afterThing2Line = afterSecondNewline.dropWhile(_ != NumLiteral("2")).drop(1)

    afterThing2Line.headOption should be (Some(Delimit()))
    afterThing2Line.drop(1).headOption should be (Some(Identifier("thing2")))

    val afterFinalExpressionLine = afterThing2Line.dropWhile(_ != Identifier("thing2")).drop(1)
    afterFinalExpressionLine.headOption should be (Some(Delimit()))
  }

  it should "prevent mixing of tabs and spaces in indentation" in {
    val results = TokenLexer(TestPrograms.invalidIndentation)
    results shouldBe a [Left[_, _]]
  }

  it should "collapse multiple adjacent delimiters" in {
    val results = TokenLexer(TestPrograms.explicitDelimiterTest)
    results shouldBe a [Right[_,_]]

    val Right(tokens) = results

    var remainingTokens = tokens
    while(remainingTokens.nonEmpty) {
      remainingTokens = remainingTokens.dropWhile(_ != Delimit()).drop(1)

      remainingTokens.headOption should not be (Some(Delimit()))
    }
  }
}
