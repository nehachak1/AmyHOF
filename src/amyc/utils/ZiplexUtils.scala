package amyc.utils

import com.mutablemaps.map.Hashable
import com.ziplex.lexer.VerifiedRegex._
import com.ziplex.lexer.MemoisationZipper
import com.ziplex.lexer.ZipperRegex._
import com.ziplex.lexer.Token
import scala.annotation.tailrec
import com.ziplex.lexer.Sequence
import com.ziplex.lexer.emptySeq
import com.ziplex.lexer.singletonSeq
import com.ziplex.lexer.seqFromList
import stainless.collection.IArray
import com.ziplex.lexer.seqFromArray

object RegexUtils:
  extension (s: String) def r: Regex[Char] = s.toCharArray().foldRight[Regex[Char]](EmptyExpr())((c, acc) => if isEmptyExpr(acc) then ElementMatch(c) else Concat(ElementMatch(c), acc))
  extension (c: Char) def r: Regex[Char] = ElementMatch(c)
  extension (r: Regex[Char]) infix def | (r2: Regex[Char]): Regex[Char] = Union(r, r2)
  extension (r: Regex[Char]) def * : Regex[Char] = Star(r)
  extension (r: Regex[Char]) def + : Regex[Char] = r ~ Star(r)
  extension (r: Regex[Char]) infix def ~ (r2: Regex[Char]): Regex[Char] = Concat(r, r2)
  extension (s: String) def * : Regex[Char] = r(s).*
  extension (s: String) def anyOf: Regex[Char] = s.toCharArray().foldRight[Regex[Char]](EmptyLang())((c, acc) => if isEmptyLang(acc) then ElementMatch(c) else Union(ElementMatch(c), acc))
  def opt(r: Regex[Char]): Regex[Char] = r | ε
  val `∅`: Regex[Char] = EmptyLang()
  val `ε`: Regex[Char] = EmptyExpr()

  extension (s: String) def toStainless: Sequence[Char] = seqFromArray(s.toCharArray().foldLeft[IArray[Char]](IArray.empty())((acc, c) => acc.append(c)))
  extension (r: Regex[Char]) def asString(): String = r match {
    case EmptyLang() => "∅"
    case EmptyExpr() => "ε"
    case ElementMatch(c) => c.toString
    case Union(r1, r2) => s"(${r1.asString()} | ${r2.asString()})"
    case Concat(r1, r2) => s"${r1.asString()}${r2.asString()}"
    case Star(r1) => s"${r1.asString()}*"
  }

  val AZString: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val azString: String = "abcdefghijklmnopqrstuvwxyz"
  val digitsString: String = "0123456789"
  val whiteSpacesString: String = " \n\t\r"
  val specialCharsString: String = "+-_/*!?=()[]{}<>|\\&%$§§°`^@#~;:,.éàèçù\'\"`"
  val allString: String = AZString + azString + digitsString + whiteSpacesString + specialCharsString

  val AZ: Regex[Char] = anyOf(AZString)
  val az: Regex[Char] = anyOf(azString)
  val azAZ: Regex[Char] = az | AZ
  val digits: Regex[Char] = anyOf(digitsString)
  val whiteSpaces: Regex[Char] = anyOf(whiteSpacesString)
  val specialChars: Regex[Char] = anyOf(specialCharsString)
  val all: Regex[Char] = anyOf(allString)

  /**
    * Creates a regex that matches any character in the interval [start, end].
    *
    * @param start the start character (inclusive)
    * @param end the end character (inclusive)
    * @return a regex that matches any character in the interval [start, end]
    */
  @tailrec
  def interval(start: Char, end: Char, acc: Regex[Char] = EmptyExpr()): Regex[Char] = {
    require(start <= end)
    if start == end then Union(ElementMatch(start), acc)
    else interval((start + 1).toChar, end, Union(ElementMatch(start), acc))
  }

  extension (t: Token[Char]) def asString(): String = 
    def replaceSpecialCharacters(l: Sequence[Char]): Sequence[String] = 
      t.charsOf.map(c => if c == '\t' then "\\t" else if c == '\n' then "\\n" else f"$c")
      
    s"Token(${t.rule.tag}, \"${replaceSpecialCharacters(t.charsOf).efficientList.mkString("")}\")"
  extension [A] (l: stainless.collection.List[A]) def mkString(inter: String) : String = l match {
    case stainless.collection.Nil() => ""
    case stainless.collection.Cons(h, t) => h.toString + (if t.isEmpty then "" else inter + t.mkString(inter))
  }
  extension (c: Context[Char]) def asStringContext(): String = s"Sequence(${c.exprs.map(regex => regex.asString()).mkString(", ")})"
  extension (z: Zipper[Char]) def asStringZipper(): String = s"Set(${z.map(c => c.asStringContext()).mkString(", ")})"


  
  extension (l: scala.collection.immutable.List[Char]) @tailrec def toStainlessList(acc: stainless.collection.List[Char] = stainless.collection.Nil[Char]()): stainless.collection.List[Char] = l match {
    case l: scala.collection.immutable.List[Char] if l.isEmpty => acc
    case l: scala.collection.immutable.List[Char] => l.tail.toStainlessList(acc :+ l.head)
  }
  extension (s: String) def toStainlessList: stainless.collection.List[Char] = s.toCharArray().toList.toStainlessList()
end RegexUtils

object SequenceUtils:
  import com.ziplex.lexer.Sequence
  import com.ziplex.lexer.emptySeq
  import com.ziplex.lexer.singletonSeq
  import com.ziplex.lexer.seqFromList
  import com.ziplex.lexer.seqFromArray
  import stainless.collection.IArray

  extension [T] (s: Sequence[T]) def mkString(inter: String): String = (0.until(s.size.toInt)).foldLeft("")((acc, i) => acc + s.apply(i).toString + (if i == s.size.toInt - 1 then "" else inter))
    

end SequenceUtils

object ZipLexUtils:
  import com.ziplex.lexer.VerifiedLexer.Lexer
  import com.ziplex.lexer.Rule
  import com.ziplex.lexer.Token
  import com.ziplex.lexer.example.ExampleUtils
  import java.io.File

  def checkRulesValidity(rules: stainless.collection.List[Rule[Char]]): Boolean =
    !rules.isEmpty && Lexer.rulesInvariant(rules)
    

  def lex(rules: stainless.collection.List[Rule[Char]], input: Sequence[Char]): (Sequence[Token[Char]], Sequence[Char]) =
    given zipperCacheUp: MemoisationZipper.CacheUp[Char] = MemoisationZipper.emptyUp(ExampleUtils.ContextCharHashable)
    given zipperCacheDown: MemoisationZipper.CacheDown[Char] = MemoisationZipper.emptyDown(ExampleUtils.RegexContextCharHashable)
    given furthestNullableCache: MemoisationZipper.CacheFurthestNullable[Char] = MemoisationZipper.emptyFurthestNullableCache(ExampleUtils.ZipperBigIntBigIntHashable, input)
    val (tokens, suffix) = Lexer.lexMem(rules, input)
    (tokens, suffix)


  /**
   * Given a starting position and the characters of a token, computes the position
   * immediately after the token.
   */
  def nextPosition(pos: Position, tokenChars: Sequence[Char]): Position =
    var line = pos.line
    var col = pos.col
    var i = 0
    while i < tokenChars.size.toInt do
      if tokenChars.apply(i) == '\n' then
        line += 1
        col = 1
      else
        col += 1
      i += 1
    end while
    SourcePosition(pos.file, line, col)

  /**
   * Given a sequence of tokens and the file they come from, returns a sequence of pairs
   * associating each token with its position in the file, and the position immediately after the last token.
   */
  def addPositions(tokens: Sequence[Token[Char]], file: File): (Sequence[(Position, Token[Char])], Position) =
    var nextPos: Position = SourcePosition(file, 1, 1)
    (tokens.map { t =>
      val tokenPos = nextPos
      val tokenChars = t.charsOf
      nextPos = nextPosition(nextPos, tokenChars)
      (tokenPos, t)
    }, nextPos)


  extension [T] (bc: Sequence[T]) def foreach(f: T => Unit): Unit =
    var i = 0
    while i < bc.size.toInt do
      f(bc.apply(i))
      i += 1
    end while
end ZipLexUtils
    