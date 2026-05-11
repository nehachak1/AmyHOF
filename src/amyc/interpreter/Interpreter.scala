package amyc
package interpreter

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier
import analyzer.SymbolTable

// An interpreter for Amy programs, implemented in Scala
                            // compiler takes (Program, SymbolTable) as input and produces Unit
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this.asInstanceOf[IntValue].i
    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b
    def asString: String = this.asInstanceOf[StringValue].s

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
    }
  }
  case class IntValue(i: Int) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case class StringValue(s: String) extends Value
  case object UnitValue extends Value
  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

          // context: for error reporting and options
          // program: AST of whole program
          // symbol table: symbol table containing type/function/constructor info
  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v
    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt")    -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString")  -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt")     -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString")   -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined
    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name
    def findFunction(owner: String, name: String) = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      }.get
    }

    // Interprets a function, using evaluations for local variables contained in 'locals'
    // TODO: Complete all missing cases. Look at the given ones for guidance. 
                              // implicit: scala automatically passes locals to every recursive call of interpret
                                       // variable name  → runtime value
    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      expr match {
        case Variable(name) => locals(name) // looks up variable value in the locals mapping
        case IntLiteral(i) => IntValue(i)
        case BooleanLiteral(b) => BooleanValue(b)
        case StringLiteral(s) => StringValue(s)
        case UnitLiteral() => UnitValue
        case Plus(lhs, rhs) => IntValue(interpret(lhs).asInt + interpret(rhs).asInt)
        case Minus(lhs, rhs) => IntValue(interpret(lhs).asInt - interpret(rhs).asInt)
        case Times(lhs, rhs) => IntValue(interpret(lhs).asInt * interpret(rhs).asInt)
        case Div(lhs, rhs) => 
          val l = interpret(lhs).asInt
          val r = interpret(rhs).asInt
          if (r == 0) ctx.reporter.fatal("Division by zero") // ctx.reporter.fatal is how to write an error message
          IntValue(l / r)
        case Mod(lhs, rhs) =>
          val l = interpret(lhs).asInt
          val r = interpret(rhs).asInt
          if (r == 0) ctx.reporter.fatal("Division by zero")
          IntValue(l % r)
        case LessThan(lhs, rhs) => BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)
        case LessEquals(lhs, rhs) => BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)
        case And(lhs, rhs) => // done this way because of the short-circuitng issue of Amy
          val left_value = interpret(lhs).asBoolean
          if (!left_value) BooleanValue(false)
          else BooleanValue(interpret(rhs).asBoolean)
        case Or(lhs, rhs) =>
          val left_value = interpret(lhs).asBoolean
          if (left_value) BooleanValue(true)
          else BooleanValue(interpret(rhs).asBoolean)
        case Equals(lhs, rhs) =>
          val v1 = interpret(lhs)
          val v2 = interpret(rhs)
          (v1, v2) match {
            case (IntValue(a), IntValue(b))         => BooleanValue(a == b)
            case (BooleanValue(a), BooleanValue(b)) => BooleanValue(a == b)
            case (UnitValue, UnitValue)             => BooleanValue(true) // unit values are always true

            // reference equality for String + ADTs (caseclassvalue thing) so eq 
            // left as _ because it doesn't really matter because we have already defined v1, v2 above
            case (_: StringValue, _ : StringValue)     => BooleanValue(v1.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef])
            case (_: CaseClassValue, _ : CaseClassValue) => BooleanValue(v1.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef])

            // default case
            case _ => BooleanValue(v1.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef])
          }
        case Concat(lhs, rhs) => StringValue(interpret(lhs).asString + interpret(rhs).asString)
        case Not(e) => BooleanValue(!interpret(e).asBoolean)
        case Neg(e) => IntValue(-interpret(e).asInt)
        case Call(qname, args) =>
            // Hint: Check if it is a call to a constructor first,
            //       then if it is a built-in function (otherwise it is a normal function).
            //       Use the helper methods provided above to retrieve information from the symbol table.
            //       Think how locals should be modified.

            // args is a List[Expr] and argVals is the list of runtime values, same order
            val argVals: List[Value] = args.map(interpret)

            // example: Call(Cons, List(1, xs)) → CaseClassValue(Cons, [1, xs])
            if (isConstructor(qname)) {
              CaseClassValue(qname, argVals)
            } else {
              // 2) function call 
              val owner: String = findFunctionOwner(qname) // determine which module owns that function (e.g. "Std")
              val fname: String = qname.name // raw function name string

              builtIns.get((owner, fname)) match {
                // if built-in: call Scala implementation with runtime args
                case Some(impl) => impl(argVals)

                case None =>
                  val fd = findFunction(owner, fname)
                  // fd.params => list of parameter defs (each has .name: Identifier)
                  // map(_.name) gives list of Identifiers
                  // .zip(argVals) pairs each param name with its passed value
                  // .toMap turns it into an environment
                  val newLocals: Map[Identifier, Value] = fd.params.map(_.name).zip(argVals).toMap
                  interpret(fd.body)(using newLocals)
              }
            }
            
        // evaluate e1 and ignore result and evaluate e2 and return it  
        case Sequence(e1, e2) =>
            interpret(e1)
            interpret(e2)
        case Let(df, value, body) =>
            val v = interpret(value) // value evaluated in old locals
            // extend locals with a new mapping from df.name → v
            // evaluate body in extended environment
            // if locals already contains df.name, the + overrides it for the body
            interpret(body)(using locals + (df.name -> v))
        case Ite(cond, thenn, elze) =>
            if (interpret(cond).asBoolean) interpret(thenn) else interpret(elze)
        case Match(scrut, cases) =>
          val evS = interpret(scrut)

          // try to match runtime value v against pattern pat
          // if success: Some(bindings). bindings => list of (Identifier -> Value) produced by variable patterns
                                                             // list not map since a pattern can bind multiple names
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) => Some(Nil)

              case (_, IdPattern(name)) => Some(List(name -> v))

              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                if (i1 == i2) Some(Nil) else None

              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                if (b1 == b2) Some(Nil) else None

              // String literal patterns never match in the amy language
              case (StringValue(_), LiteralPattern(StringLiteral(_))) => None

              case (UnitValue, LiteralPattern(UnitLiteral())) => Some(Nil)

              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                if (con1 != con2 || realArgs.length != formalArgs.length) then None
                else {
                  // recursively match each runtime arg rv with corresponding pattern arg fp
                  // ensures any mismatch kills entire match
                                                                // so far successful, no bindings
                  val init: Option[List[(Identifier, Value)]] = Some(Nil)
                  (realArgs zip formalArgs).foldLeft(init) {
                    case (None, _) => None // failure
                    case (Some(acc), (rv, fp)) => matchesPattern(rv, fp).map(acc ++ _)
                  }
                }
              
            }
          }

          cases.to(LazyList).map { matchCase =>
            val MatchCase(pat, rhs) = matchCase
            (rhs, matchesPattern(evS, pat))
          }.find(_._2.isDefined) match {
            case Some((rhs, Some(moreLocals))) =>
              interpret(rhs)(using locals ++ moreLocals)
            case _ =>
              ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")
          }
      case Error(msg) => 
        // evaluate message expression
        val m = interpret(msg).asString
        // aborting program code - shows error message expression
        ctx.reporter.fatal(s"Error: $m")    
    }
  }
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpret(e)(using Map())
    }
  }
}
