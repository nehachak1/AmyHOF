package amyc
package parsing

import amyc.utils.Positioned

sealed trait Token extends Positioned with Product {
  override def toString = {
    productPrefix + productIterator.mkString("(", ",", ")") + "(" + position.withoutFile + ")"
  }
}

object Tokens {
  final case class KeywordToken(value: String) extends Token    // e.g. keyword "if"
  final case class IdentifierToken(name: String) extends Token  // e.g. variable name "x" 
  final case class PrimTypeToken(value: String) extends Token   // e.g. primitive type "Int"
  final case class IntLitToken(value: Int) extends Token        // e.g. integer literal "123"
  final case class StringLitToken(value: String) extends Token
  final case class BoolLitToken(value: Boolean) extends Token
  final case class DelimiterToken(value: String) extends Token  // .,:;(){}[]= and => and :=
  final case class OperatorToken(name: String) extends Token    // e.g. "+"
  final case class CommentToken(text: String) extends Token     // e.g. "// this is a comment"
  final case class SpaceToken() extends Token                   // e.g. "\n  "
  final case class ErrorToken(content: String) extends Token
  // Parser-only token for lambda-starting "(".
  // The lexer does not produce this directly; Parser.run rewrites only typed-lambda openings to this token.
  // We need this because normal parentheses and lambdas both start with `(`.
  // Example normal parentheses: `(x + 1)`.
  // Example lambda parentheses: `(x: Int(32)) => x + 1`.
  // Giving lambdas their own token kind helps Scallion keep the grammar LL(1).
  final case class LambdaOpenToken() extends Token
  final case class EOFToken() extends Token                     // special token at the end of file
}

sealed abstract class TokenKind(representation: String) {
  override def toString: String = representation
}

object TokenKinds {
  final case class KeywordKind(value: String) extends TokenKind(value)
  case object IdentifierKind extends TokenKind("<Identifier>")
  case object PrimTypeKind extends TokenKind("<Primitive Type>")
  case object LiteralKind extends TokenKind("<Literal>")
  final case class DelimiterKind(value: String) extends TokenKind(value)
  final case class OperatorKind(value: String) extends TokenKind(value)
  // Separate kind for lambda openings, so Scallion can distinguish `(x: T) => ...`
  // from ordinary parentheses while keeping the grammar LL(1).
  // This kind corresponds to LambdaOpenToken above.
  // It is not part of the original Amy lexer output; it only exists inside the parser phase.
  case object LambdaOpenKind extends TokenKind("<Lambda Open>")
  case object EOFKind extends TokenKind("<EOF>")
  case object NoKind extends TokenKind("<???>")
}

object TokenKind {
  import Tokens._
  import TokenKinds._

  def of(token: Token): TokenKind = token match {
    case KeywordToken(value) => KeywordKind(value)
    case IdentifierToken(_) => IdentifierKind
    case PrimTypeToken(_) => PrimTypeKind
    case BoolLitToken(_) => LiteralKind
    case IntLitToken(_) => LiteralKind
    case StringLitToken(_) => LiteralKind
    case DelimiterToken(value) => DelimiterKind(value)
    case OperatorToken(value) => OperatorKind(value)
    // Parser.run converts selected DelimiterToken("(") tokens into LambdaOpenToken().
    // Once that happens, Scallion sees LambdaOpenKind and chooses the lambda parser rule.
    case LambdaOpenToken() => LambdaOpenKind
    case EOFToken() => EOFKind
    case _ => NoKind
  }
}
