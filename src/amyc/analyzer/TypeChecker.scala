package amyc
package analyzer

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] = 
        List(Constraint(found, expected, e.position))

      e match {
        case IntLiteral(_) => 
          topLevelConstraint(IntType)

        case StringLiteral(_) => 
          topLevelConstraint(StringType)

        case BooleanLiteral(_) => 
          topLevelConstraint(BooleanType)

        case Variable(id) => 
          topLevelConstraint(env(id))

        case UnitLiteral() => 
          topLevelConstraint(UnitType)

        case Plus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType) ++
          topLevelConstraint(IntType)

        case Minus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType) ++
          topLevelConstraint(IntType)

        case Times(lhs, rhs) =>
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType) ++
          topLevelConstraint(IntType)

        case Div(lhs, rhs) =>
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType) ++
          topLevelConstraint(IntType)

        case Mod(lhs, rhs) =>
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType) ++
          topLevelConstraint(IntType)

        case And(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++
          genConstraints(rhs, BooleanType) ++
          topLevelConstraint(BooleanType)

        case Or(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++
          genConstraints(rhs, BooleanType) ++
          topLevelConstraint(BooleanType)

        case Equals(lhs, rhs) =>
          val tv = TypeVariable.fresh()
          genConstraints(lhs, tv) ++
          genConstraints(rhs, tv) ++
          topLevelConstraint(BooleanType)

        case LessThan(lhs, rhs) =>
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType) ++
          topLevelConstraint(BooleanType)

        case LessEquals(lhs, rhs) =>
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType) ++
          topLevelConstraint(BooleanType)

        case Concat(lhs, rhs) =>
          genConstraints(lhs, StringType) ++
          genConstraints(rhs, StringType) ++
          topLevelConstraint(StringType)

        case Neg(e) =>
          genConstraints(e, IntType) ++
          topLevelConstraint(IntType)
        
        case Not(e) =>
          genConstraints(e, BooleanType) ++
          topLevelConstraint(BooleanType)

        case Ite(cond, thenn, elze) =>
          genConstraints(cond, BooleanType) ++
          genConstraints(thenn, expected) ++
          genConstraints(elze, expected)

        case Let(df, value, body) =>
          val tv = TypeVariable.fresh()
          genConstraints(value, tv) ++
          genConstraints(body, expected)(using env + (df.name -> tv)) ++
          List(Constraint(tv, df.tt.tpe, e.position))
        
        case Sequence(e1, e2) =>
          val tv = TypeVariable.fresh()
          genConstraints(e1, tv) ++
          genConstraints(e2, expected)

        case Call(id, args) =>
          table.getFunction(id) match {
            case Some(fd) => 
              args.zip(fd.argTypes).flatMap { case (arg, tpe) =>
                genConstraints(arg, tpe)
              } ++ topLevelConstraint(fd.retType)
            case None =>
              table.getConstructor(id) match {
                case Some(cons) =>
                  args.zip(cons.argTypes).flatMap { case (arg, tpe) =>
                    genConstraints(arg, tpe)
                  } ++ topLevelConstraint(cons.retType)

                case None =>
                  ctx.reporter.error(s"Unknown function or constructor: $id", e.position)
                  Nil
              }
          }

        case Error(msg) => 
          genConstraints(msg, StringType) ++
          topLevelConstraint(expected)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def patternBindings(pat: Pattern, expected: Type): (List[Constraint], Map[Identifier, Type]) = pat match {

              case WildcardPattern() => 
                (Nil, Map())

              case IdPattern(id) => 
                (Nil, Map(id -> expected))

              case LiteralPattern(IntLiteral(_)) =>
                (List(Constraint(IntType, expected, pat.position)), Map())

              case LiteralPattern(StringLiteral(_)) =>
                (List(Constraint(StringType, expected, pat.position)), Map())

              case LiteralPattern(BooleanLiteral(_)) =>
                (List(Constraint(BooleanType, expected, pat.position)), Map())

              case LiteralPattern(UnitLiteral()) =>
                (List(Constraint(UnitType, expected, pat.position)), Map())

              case CaseClassPattern(constr, args) =>
                val cons = table.getConstructor(constr).get

                val base = List(Constraint(cons.retType, expected, pat.position))

                val sub = args.zip(cons.argTypes).map { case (p, tpe) =>
                  patternBindings(p, tpe)
                }

                val subConstraints = sub.flatMap(_._1)
                val subEnv = sub.flatMap(_._2).toMap
                (base ++ subConstraints, subEnv)

          }    

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = patternBindings(cse.pat, scrutExpected)
            patConstraints ++
            genConstraints(cse.expr, expected)(using env ++ moreEnv)
          }

           val st = TypeVariable.fresh()
           genConstraints(scrut, st) ++
           cases.flatMap(cse => handleCase(cse, st)) ++
           topLevelConstraint(expected)
      }
    }

    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Do a single substitution.
    def subst(tpe: Type, from: Int, to: Type): Type = {
      tpe match {
        case TypeVariable(`from`) => to
        case other => other
      }
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
            // TODO clea
          //go through each constraint type of the list of constraints so recursive function
          //get 3 possibilities :
            // - match : same types
            // - substitution : different types but that matcj like Int and IntLiteral, replace all T with its type
                  // asignment x = 1 so x becomes an Int so substitute x with Int
            // - failure : different types
          /* 3 types :
            * base type : Int, Bool, String
            * proper name : List, Option
           */

          /* Do not end the execution as soon as an error occurs! Instead, 
          collect all the errors and report them at the end of the type checking phase.*/
          (found, expected) match {

            //matches
            /**case (Int, Int) => solveConstraints(more)
            case (Boolean, Boolean) => solveConstraints(more)
            case (String, String) => solveConstraints(more)*/
            case (type1, type2) if (type1 == type2) => 
              solveConstraints(more)

            //substitution
            case (TypeVariable(type_v1), type_v2) => //substitute case replace tyep variables
               solveConstraints(subst_*(more, type_v1, type_v2)) //recrusively call on rest

            case (type_v1, TypeVariable(type_v2)) => 
              solveConstraints(subst_*(more, type_v2, type_v1)) //recrusively call on rest
            
            //failure such as 2 different types or different class types
            /* Do not end the execution as soon as an error occurs! Instead, 
          collect all the errors and report them at the end of the type checking phase.*/
            case _ => 
              ctx.reporter.error("Both types do not match, the expected is : " + expected + " and the found is : " + found);
              solveConstraints(more)

          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(using env))
      }

      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(using Map())))
    }

    v

  }
}
