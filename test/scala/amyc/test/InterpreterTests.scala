package amyc.test

import amyc.parsing._
import amyc.analyzer._
import amyc.interpreter.Interpreter
import amyc.utils._
import org.junit.Test 

class InterpreterTests extends ExecutionTests {

  val pipeline =
    AmyLexer // AmyLexer
    .andThen(Parser)
    .andThen(NameAnalyzer)
    .andThen(TypeChecker)
    .andThen(Interpreter) 
}