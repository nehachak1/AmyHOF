package amyc.ast

import scala.language.implicitConversions
import amyc.utils._

// A printer for Amy trees
trait Printer {

  val treeModule: TreeModule
  import treeModule._

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document
  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document

  protected implicit def stringToDoc(s: String): Raw = Raw(s)

  def apply(t: Tree)(implicit printUniqueIDs: Boolean = false): String = {

    def binOp(e1: Expr, op: String, e2: Expr) = "(" <:> rec(e1) <:> " " + op + " " <:> rec(e2) <:> ")"

    def rec(t: Tree, parens: Boolean = true): Document = t match {
      /* Definitions */
      case Program(modules) =>
        Stacked(modules map (rec(_)), emptyLines = true)

      case ModuleDef(name, defs, optExpr) =>
        Stacked(
          "object " <:> name,
          "",
          Indented(Stacked(defs ++ optExpr.toList map (rec(_, false)), emptyLines = true)),
          "end " <:> name,
          ""
        )

      case AbstractClassDef(name) =>
        "abstract class " <:> printName(name)

      case CaseClassDef(name, fields, parent) =>
        def printField(f: TypeTree) = "v: " <:> rec(f)
        "case class " <:> name <:> "(" <:> Lined(fields map printField, ", ") <:> ") extends " <:> parent

      case FunDef(name, params, retType, body) =>
        Stacked(
          "def " <:> name <:> "(" <:> Lined(params map (rec(_)), ", ") <:> "): " <:> rec(retType) <:> " :=",
          Indented(rec(body, false)),
          "end " <:> name
        )

      case ParamDef(name, tpe) =>
        name <:> ": " <:> rec(tpe)

      /* Expressions */
      case Variable(name) =>
        name
      case IntLiteral(value) =>
        value.toString
      case BooleanLiteral(value) =>
        value.toString
      case StringLiteral(value) =>
        "\"" + value + '"'
      case UnitLiteral() =>
        "()"
      case Plus(lhs, rhs) =>
        binOp(lhs, "+", rhs)
      case Minus(lhs, rhs) =>
        binOp(lhs, "-", rhs)
      case Times(lhs, rhs) =>
        binOp(lhs, "*", rhs)
      case Div(lhs, rhs) =>
        binOp(lhs, "/", rhs)
      case Mod(lhs, rhs) =>
        binOp(lhs, "%", rhs)
      case LessThan(lhs, rhs) =>
        binOp(lhs, "<", rhs)
      case LessEquals(lhs, rhs) =>
        binOp(lhs, "<=", rhs)
      case And(lhs, rhs) =>
        binOp(lhs, "&&", rhs)
      case Or(lhs, rhs) =>
        binOp(lhs, "||", rhs)
      case Equals(lhs, rhs) =>
        binOp(lhs, "==", rhs)
      case Concat(lhs, rhs) =>
        binOp(lhs, "++", rhs)
      case Not(e) =>
        "!(" <:> rec(e) <:> ")"
      case Neg(e) =>
        "-(" <:> rec(e) <:> ")"
      case Call(name, args) =>
        name <:> "(" <:> Lined(args map (rec(_)), ", ") <:> ")"
      case Apply(fun, args) =>
        // Higher-order call: print the function expression first, then the argument list
        // This keeps examples such as `makeAdder(5)(10)` readable after parsing/name analysis
        // `rec(fun)` prints the expression that evaluates to a function
        // For `makeAdder(5)(10)`, `rec(fun)` prints `makeAdder(5)`
        // Then we print one more argument list, which gives the final `(10)`
        // `Lined(args map (rec(_)), ", ")` means "print every argument and separate them with commas"
        rec(fun) <:> "(" <:> Lined(args map (rec(_)), ", ") <:> ")"
      case Lambda(params, body) =>
        // Anonymous function syntax introduced for higher-order functions
        // We print the parameters between parentheses, then `=>`, then the body
        // This mirrors the syntax accepted by the parser: `(x: Int(32)) => x + 1`
        // Each parameter is printed using the existing ParamDef printer case below
        // The body uses `rec(body)`, so nested expressions still get printed normally
        "(" <:> Lined(params map (rec(_)), ", ") <:> ") => " <:> rec(body)
      case Sequence(lhs, rhs) =>
        val main = Stacked(
          rec(lhs, false) <:> ";",
          rec(rhs, false),
        )
        if (parens) {
          Stacked(
            "(",
            Indented(main),
            ")"
          )
        } else {
          main
        }
      case Let(df, value, body) =>
        val main = Stacked(
          "val " <:> rec(df) <:> " =",
          Indented(rec(value)) <:> ";",
          rec(body, false) // For demonstration purposes, the scope or df is indented (what does this even mean??)
        )
        if (parens) {
          Stacked(
            "(",
            Indented(main),
            ")"
          )
        } else {
          main
        }
      case Ite(cond, thenn, elze) =>
        Stacked(
          "(if(" <:> rec(cond) <:> ") then",
          Indented(rec(thenn)),
          "else",
          Indented(rec(elze)),
          "end if)"
        )
      case Match(scrut, cases) =>
        Stacked(
          rec(scrut) <:> " match {",
          Indented(Stacked(cases map (rec(_)))),
          "}"
        )
      case Error(msg) =>
        "error(" <:> rec(msg) <:> ")"

      /* cases and patterns */
      case MatchCase(pat, expr) =>
        Stacked(
          "case " <:> rec(pat) <:> " =>",
          Indented(rec(expr))
        )
      case WildcardPattern() =>
        "_"
      case IdPattern(name) =>
        name
      case LiteralPattern(lit) =>
        rec(lit)
      case CaseClassPattern(name, args) =>
        name <:> "(" <:> Lined(args map (rec(_)), ", ") <:> ")"

      /* Types */
      case TypeTree(tp) =>
        tp match {
          case IntType => "Int(32)"
          case BooleanType => "Boolean"
          case StringType => "String"
          case UnitType => "Unit"
          case ClassType(name) => name
          case FunctionType(args, ret) =>
            // Function types are printed in the same syntax the parser accepts
            // Single argument: `Int(32) => Boolean`; multiple arguments: `(Int(32), String) => Boolean`
            // We special-case one argument only to avoid unnecessary parentheses
            // This makes the common type `Int(32) => Int(32)` look like the proposal
            // For 2+ arguments, parentheses are required so the reader knows all those types are inputs
            val printedArgs =
              // One input type: print just that type, e.g. `Int(32)`
              if (args.size == 1) rec(TypeTree(args.head))
              // Several input types: print `(A, B, C)`
              else "(" <:> Lined(args map (tpe => rec(TypeTree(tpe))), ", ") <:> ")"
            // Finally print the arrow and the return type
            printedArgs <:> " => " <:> rec(TypeTree(ret))
        }

    }

    rec(t).print
  }
}

object NominalPrinter extends Printer {
  val treeModule: NominalTreeModule.type = NominalTreeModule
  import NominalTreeModule._

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = Raw(name)

  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document = {
    Raw(name match {
      case QualifiedName(Some(module), name) =>
        s"$module.$name"
      case QualifiedName(None, name) =>
        name
    })
  }
}

object SymbolicPrinter extends SymbolicPrinter
trait SymbolicPrinter extends Printer {
  val treeModule: SymbolicTreeModule.type = SymbolicTreeModule
  import SymbolicTreeModule._

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = {
    if (printUniqueIds) {
      name.fullName
    } else {
      name.name
    }
  }

  @inline implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document = {
    printName(name)
  }
}
