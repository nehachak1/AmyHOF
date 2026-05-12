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
  inputFiles = List("HOFCompose"), 
  outputFile = "HOFCompose", 
  libraryFiles = List("Std", "Option", "List") 
  )

  /* Test to verify that the map function works */
  @Test def testHOFMap = shouldOutput(
  inputFiles = List("HOFMap"), 
  outputFile = "HOFMap", 
  libraryFiles = List("Std", "Option", "List") 
  )
  
  /* Test to verify that the FoldLeft function works */
  @Test def testHOFFold = shouldOutput(
  inputFiles = List("HOFFold"), 
  outputFile = "HOFFold", 
  libraryFiles = List("Std", "Option", "List") 
  )
}