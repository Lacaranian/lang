package com.schemagames.lang.parser

import com.schemagames.lang.syntax.Token
import com.schemagames.lang.syntax.Tokens.{Delimit, Indent, Outdent}

// Some post-processing to massage a sequence of Tokens into a form
object LexerToParser {
  def apply(tokens: List[Token]): List[Token] = tokens.flatMap({
    case Indent() => None
    case Outdent() => Some(Delimit())
    case other => Some(other)
  })
}
