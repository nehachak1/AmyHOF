package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._
import java.lang.reflect.Parameter
import javax.lang.model.element.QualifiedNameable

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  val lambdaOpen: Syntax[Token] = elem(LambdaOpenKind)
  def op(string: String): Syntax[Token] = elem(OperatorKind(string))
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  /*
  Lazy val recap: 
    EX: lazy val x = {
          println("computing x")
          42
        }

        println("before")
        println(x)
        println(x)

        output: // "computing x" appears only once, once x is calculated, result is directly printed second time
        before
        computing x
        42
        42

  */

  // lazy val valExpr = "recipe to build a parser later"
  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ many(definition) ~ opt(expr) ~ kw("end") ~ identifier).map {
    case obj ~ id ~ defs ~ body ~ _ ~ id1 => 
      if id == id1 then 
        ModuleDef(id, defs.toList, body).setPos(obj)
      else 
        throw new AmycFatalError("Begin and end module names do not match: " + id + " and " + id1)
  }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module. 
  /* a function can be a normal function, an abstract class or a case class */
  lazy val definition: Syntax[ClassOrFunDef] = 
    functionDefinition | abstractClassDefinition | caseClassDefinition
  
  //(AbstractClassDef | CaseClassDef | FunDef) 

  /* {abstract}{space(ignored)}{class}{space(ignored)}{id}{}*/
  //abstract and class are keywords
  lazy val abstractClassDefinition: Syntax[ClassOrFunDef] = 
    (kw("abstract") ~ kw("class") ~ identifier).map {
      case pos ~ _ ~ id => AbstractClassDef(id).setPos(pos)
    }

  /* {case}{space(ignored)}{class}{space(ignored)}{id}{"("}{parameters}{")"}
      {extends}{module}*/
  //All case classes have to extend a class in the same module.
  lazy val caseClassDefinition: Syntax[ClassOrFunDef] =
  (kw("case") ~ kw("class") ~ identifier ~ "(" ~ parameters ~ ")" ~ kw("extends") ~ identifier).map {
    case pos ~ _ ~ id1 ~ _ ~ paramets ~ _ ~ _ ~ id2 =>
      CaseClassDef(id1, paramets.map(_.tt), id2).setPos(pos)
  }

  lazy val functionDefinition: Syntax[ClassOrFunDef] =
  (kw("def") ~ identifier ~ "(" ~ parameters ~ ")" ~ ":" ~ typeTree ~ ":=" ~ expr ~ kw("end") ~ identifier).map {
    case pos ~ id ~ _ ~ params ~ _ ~ _ ~ retType ~ _ ~ body ~ _ ~ _ =>
      FunDef(id, params, retType, body).setPos(pos)
  }

  // A list of parameter definitions.s
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type. 
  /* a paremeter is seen as a ({identifier} {":"} {expected type} */
  lazy val parameter: Syntax[ParamDef] = 
    //create the AST node from the parameters
   (identifierPos ~ ":" ~ typeTree).map{
    case (id, pos) ~ colon ~ type_tree => ParamDef(id, type_tree).setPos(pos)}

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = recursive {
    tupleFunctionType | singleFunctionType
  }

  lazy val typeAtom: Syntax[TypeTree] = primitiveType | identifierType

  lazy val tupleFunctionType: Syntax[TypeTree] =
    ("(" ~>~ repsep(typeTree, ",") ~<~ ")" ~ "=>" ~ typeTree).map {
      case args ~ _ ~ ret =>
        TypeTree(FunctionType(args.toList.map(_.tpe), ret.tpe)).setPos(args.headOption.getOrElse(ret))
    }

  lazy val singleFunctionType: Syntax[TypeTree] =
    (typeAtom ~ opt("=>" ~>~ typeTree)).map {
      case arg ~ Some(ret) =>
        TypeTree(FunctionType(List(arg.tpe), ret.tpe)).setPos(arg)
      case arg ~ None =>
        arg
    }

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = (accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new AmycFatalError("Unexpected primitive type name: " + name)
    }).setPos(tk)
  } ~ opt("(" ~ literal ~ ")")).map { 
    case (prim@TypeTree(IntType)) ~ Some(_ ~ IntLiteral(32) ~ _) => prim
    case TypeTree(IntType) ~ Some(_ ~ IntLiteral(width) ~ _) => 
      throw new AmycFatalError("Int type can only be used with a width of 32 bits, found : " + width)
    case TypeTree(IntType) ~ Some(_ ~ lit ~ _) =>
      throw new AmycFatalError("Int type should have an integer width (only 32 bits is supported)")
    case TypeTree(IntType) ~ None => 
      throw new AmycFatalError("Int type should have a specific width (only 32 bits is supported)")
    case prim ~ Some(_) => 
      throw new AmycFatalError("Only Int type can have a specific width")
    case prim ~ None => prim
  }

  // A user-defined type (such as `List`). 
  /* Represented as {id}{.}{id} */
  /* or as {id} id as in a type */
  lazy val identifierType: Syntax[TypeTree] = 
    (identifierPos  ~ opt("." ~>~ identifier)).map{
      case (id, pos) ~ None => TypeTree(ClassType(QualifiedName(None,id))).setPos(pos)
      case (id1, pos) ~ Some(id2) => TypeTree(ClassType(QualifiedName(Some(id1),id2))).setPos(pos)
    }


  // An expression.
  // Lowest-precedence layer handles `val` and `;`.
  // EX: val x: Int(32) = 1 + 2; x * 3
  // Let(x, 1 + 2, x * 3)
  lazy val expr: Syntax[Expr] = recursive { // recursive since expressions can contain expressions
    lambdaExpr | valExpr | seqExpr
  }

  // A literal expression.
  lazy val literal: Syntax[Literal[?]] =
    accept(LiteralKind){
      case tk @ IntLitToken(v) => IntLiteral(v).setPos(tk)
      case tk @ StringLitToken(v) => StringLiteral(v).setPos(tk)
      case tk @ BoolLitToken(v) => BooleanLiteral(v).setPos(tk)
    }

  // A pattern as part of a mach case. 
  /* Id(Patterns) — case class pattern
    *Id — identifier pattern
    *Literal — literal pattern
    * _ — wildcard pattern */

  lazy val pattern: Syntax[Pattern] = recursive { 
    wildPattern | literalPattern | unitPattern | variableVSconstructorPattern 
  }

  /* Case where you have case true =>  ; or case "hello" => ;*/
  lazy val literalPattern: Syntax[Pattern] = 
    (literal).map{
      case literal_pattern => LiteralPattern(literal_pattern).setPos(literal_pattern)
    }

  /* Case where case _ => ; */
  lazy val wildPattern: Syntax[Pattern] = 
    (kw("_")).map{
      case wild_pattern => WildcardPattern().setPos(wild_pattern)
    }
  
  /* Case where : case () => ; so unit  */
  /* since it's the case with "()" meaning it's composed of "(" and ')" it means we have 2 tokens
  so it can't be literal pattern*/
  lazy val unitPattern: Syntax[Pattern] =
    ("(" ~ ")").map { case l ~ _ =>
      LiteralPattern(UnitLiteral()).setPos(l)
    }
  
  /* Remaining cases where you have case Cons(a,b) => ; or case x => ; 
  so variables and constructors*/
  /*Id(Patterns) — case class pattern
    *Id — identifier pattern*/
  lazy val variableVSconstructorPattern : Syntax[Pattern] =
    (identifierPos ~ opt("." ~>~ identifier) ~ opt("(" ~>~ repsep(pattern, ",") ~<~ ")")).map {
      /* case where we only have a variable */
      case (id, pos) ~ None ~ None => IdPattern(id).setPos(pos)
      
      /* case where we have a constructor and arguments*/
      case (id, pos) ~ None ~ Some(args) =>CaseClassPattern(QualifiedName(None, id), args.toList).setPos(pos)
      
      /* case where we have a constructor that belongs to a module like Str.Cons('a','b') */
      case (mod, pos) ~ Some(id) ~ Some(args) => CaseClassPattern(QualifiedName(Some(mod), id), args.toList).setPos(pos)

      /* case where we have a constructor that belongs to a module but that constructor doesn't come with arguments,
      which is invalid */
      case (mod, _) ~ Some(id) ~ None => throw new AmycFatalError("A construcor owned by a module must be passed arguments, " + mod + "." + id + "is missing arguments")
      
      
    }

  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  // For example, if parsing:
    // a + b * c, you want:
      // a, b, c to first be parsed as simple expressions then operators combine them according to precedence so simpleExpr is the base layer

  lazy val parenOrUnitExpr: Syntax[Expr] =
    ("(" ~ opt(expr) ~ ")").map {
      case l ~ None ~ _ =>
        UnitLiteral().setPos(l)

      case _ ~ Some(e) ~ _ =>
        e
    }

  lazy val simpleExpr: Syntax[Expr] =
    literal.up[Expr] |
    variableOrCall |
    errorExpr |
    parenOrUnitExpr

  lazy val lambdaExpr: Syntax[Expr] =
    (lambdaOpen ~ parameters ~ ")" ~ "=>" ~ expr).map {
      case tok ~ params ~ _ ~ _ ~ body =>
        Lambda(params, body).setPos(tok)
    }

  lazy val argumentList: Syntax[List[Expr]] =
    ("(" ~>~ repsep(expr, ",") ~<~ ")").map(_.toList)

  lazy val variableOrCall: Syntax[Expr] =
    (identifierPos ~ opt("." ~>~ identifier) ~ many(argumentList)).map {
      case (name, pos) ~ None ~ argLists =>
        val base = Variable(name).setPos(pos)
        argLists.foldLeft(base: Expr) { case (fun, args) =>
          Apply(fun, args).setPos(fun)
        }

      case (mod, pos) ~ Some(name) ~ argLists =>
        argLists.toList match {
          case first :: rest =>
            val firstCall = Call(QualifiedName(Some(mod), name), first).setPos(pos)
            rest.foldLeft(firstCall: Expr) { case (fun, args) =>
              Apply(fun, args).setPos(fun)
            }
          case Nil =>
            throw new AmycFatalError("Qualified identifier in expression must be a call: " + mod + "." + name)
        }
    }

  lazy val errorExpr: Syntax[Expr] =
    // parses the error keyword
                   // parses the expression inside parentheses
    (kw("error") ~ ("(" ~>~ expr ~<~ ")")).map { // converts result into AST
      case tok ~ msg => Error(msg).setPos(tok)
    }

  lazy val ifExpr: Syntax[Expr] =
    (kw("if") ~ ("(" ~>~ expr ~<~ ")") ~ kw("then") ~ expr ~ kw("else") ~ expr ~ kw("end") ~ kw("if")).map {
      case tok ~ cond ~ _ ~ thenn ~ _ ~ elze ~ _ ~ _ =>
        Ite(cond, thenn, elze).setPos(tok)
    }

  lazy val matchCase: Syntax[MatchCase] =
    // EX: case Cons(h, t) => 1 + length(t)
    (kw("case") ~ pattern ~ "=>" ~ expr).map {
      case tok ~ pat ~ _ ~ rhs => MatchCase(pat, rhs).setPos(tok)
    }

  // `match` is lower precedence than binary operators and can be chained.
  
  lazy val matchExpr: Syntax[Expr] = 
  ((ifExpr | binaryExpr) ~ many(kw("match") ~ ("{" ~>~ many1(matchCase) ~<~ "}"))).map {
    case base ~ matches => matches.foldLeft(base) {
      case (scrut, _ ~ cases) => Match(scrut, cases.toList).setPos(scrut)
    }
  }
  // `;` is parsed right-associatively.c
  // EX: x; y; z
  // Sequence(x, Sequence(y, z))
  lazy val seqExpr: Syntax[Expr] =
    (matchExpr ~ opt(";" ~>~ expr)).map {
      case lhs ~ Some(rhs) => Sequence(lhs, rhs).setPos(lhs)
      case lhs ~ None => lhs
    }

  // `val` binds as little as possible before the first `;` and is not allowed in value position.
  // EX: val x: Int(32) = y; z; x
  // becomes Let(x, y, Sequence(z, x)) not Let(x, Sequence(y, z), x)
  lazy val valExpr: Syntax[Expr] =
    (kw("val") ~ parameter ~ "=" ~ matchExpr ~ ";" ~ expr).map {
      case tok ~ df ~ _ ~ value ~ _ ~ body =>
        Let(df, value, body).setPos(tok)
    }

  // Binary operators, grouped from highest to lowest precedence.
  lazy val binaryExpr: Syntax[Expr] = operators(unaryExpr)(
    // precendence levels
    (op("*") | op("/") | op("%")).is(LeftAssociative),
    (op("+") | op("-") | op("++")).is(LeftAssociative),
    (op("<") | op("<=")).is(LeftAssociative),
    op("==").is(LeftAssociative),
    op("&&").is(LeftAssociative),
    op("||").is(LeftAssociative))
    {
    // once Scallion decides which operator it found, one must say which AST node to build
    case (lhs, tok, rhs) => tok match {
      case OperatorToken("*")  => Times(lhs, rhs).setPos(lhs)
      case OperatorToken("/")  => Div(lhs, rhs).setPos(lhs)
      case OperatorToken("%")  => Mod(lhs, rhs).setPos(lhs)

      case OperatorToken("+")  => Plus(lhs, rhs).setPos(lhs)
      case OperatorToken("-")  => Minus(lhs, rhs).setPos(lhs)
      case OperatorToken("++") => Concat(lhs, rhs).setPos(lhs)

      case OperatorToken("<")  => LessThan(lhs, rhs).setPos(lhs)
      case OperatorToken("<=") => LessEquals(lhs, rhs).setPos(lhs)
      case OperatorToken("==") => Equals(lhs, rhs).setPos(lhs)

      case OperatorToken("&&") => And(lhs, rhs).setPos(lhs)
      case OperatorToken("||") => Or(lhs, rhs).setPos(lhs)
      case other => throw new AmycFatalError(s"Unexpected binary operator token: $other")
    }
  }

  // unary operators can only apply directly to simple expressions
  lazy val unaryExpr: Syntax[Expr] =
    (op("!") ~ simpleExpr).map {
      case tok ~ e => Not(e).setPos(tok)
    } |
    (op("-") ~ simpleExpr).map {
      case tok ~ e => Neg(e).setPos(tok)
    } |
    simpleExpr
  

  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    def prepareLambdaOpenTokens(tokens: Iterator[Token]): Iterator[Token] = {
      val ts = tokens.toList

      def isOpen(tok: Token): Boolean = tok match {
        case DelimiterToken("(") | LambdaOpenToken() => true
        case _ => false
      }

      def isClose(tok: Token): Boolean = tok match {
        case DelimiterToken(")") => true
        case _ => false
      }

      def matchingClose(from: Int): Option[Int] = {
        var depth = 0
        var i = from
        while (i < ts.size) {
          if (isOpen(ts(i))) depth += 1
          else if (isClose(ts(i))) {
            depth -= 1
            if (depth == 0) return Some(i)
          }
          i += 1
        }
        None
      }

      def hasTopLevelColon(from: Int, until: Int): Boolean = {
        var depth = 0
        var i = from
        while (i < until) {
          ts(i) match {
            case tok if isOpen(tok) => depth += 1
            case tok if isClose(tok) => depth -= 1
            case DelimiterToken(":") if depth == 0 => return true
            case DelimiterToken(",") if depth == 0 => return false
            case _ =>
          }
          i += 1
        }
        false
      }

      def looksLikeLambdaParams(from: Int, until: Int): Boolean = {
        if (from == until) false
        else {
          var start = from
          var i = from
          var depth = 0
          while (i <= until) {
            val atEnd = i == until
            if (!atEnd) {
              ts(i) match {
                case tok if isOpen(tok) => depth += 1
                case tok if isClose(tok) => depth -= 1
                case _ =>
              }
            }

            if (atEnd || (depth == 0 && ts(i) == DelimiterToken(","))) {
              if (start >= i || !ts(start).isInstanceOf[IdentifierToken] || !hasTopLevelColon(start + 1, i)) {
                return false
              }
              start = i + 1
            }
            i += 1
          }
          true
        }
      }

      def isLambdaOpenAt(i: Int): Boolean = ts(i) match {
        case DelimiterToken("(") =>
          matchingClose(i).exists { close =>
            close + 1 < ts.size &&
            ts(close + 1) == DelimiterToken("=>") &&
            looksLikeLambdaParams(i + 1, close)
          }
        case _ => false
      }

      ts.indices.iterator.map { i =>
        if (isLambdaOpenAt(i)) LambdaOpenToken().setPos(ts(i))
        else ts(i)
      }
    }

    try {
      parser(prepareLambdaOpenTokens(tokens)) match {
        case Parsed(result, rest) => result
        case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
        case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
      }
    } catch {
      case e: AmycFatalError =>
        ctx.reporter.fatal(e.msg)
        sys.exit(1)
    }
  }
}
