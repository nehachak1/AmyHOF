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

  /* Test to verify that the compose function works */
  @Test def testHOFCompose = shouldOutput(
  List("Std", "Option", "List", "HOFCompose"),
  "HOFCompose"
  )

  /* Test to verify that the map function works */
  @Test def testHOFMap = shouldOutput(
    List("Std", "Option", "List", "HOFMap"),
    "HOFMap"
  )

  /* Test to verify that the FoldLeft function works */
  @Test def testHOFFold = shouldOutput(
    List("Std", "Option", "List", "HOFFold"),
    "HOFFold"
  )
}