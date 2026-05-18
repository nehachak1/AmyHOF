# CLP Lab 6 - Higher-Order Functions for Amy

This project extends the Amy compiler with higher-order functions. Functions can be passed as arguments, returned from other functions, stored in variables, and written as anonymous functions.

## Modified or Added Files

Front end:

- `src/amyc/ast/TreeModule.scala`
- `src/amyc/ast/Printer.scala`
- `src/amyc/parsing/Tokens.scala`
- `src/amyc/parsing/Parser.scala`
- `src/amyc/analyzer/NameAnalyzer.scala`
- `src/amyc/analyzer/TypeChecker.scala`

Interpreter:

- `src/amyc/interpreter/Interpreter.scala`

WebAssembly code generation:

- `src/amyc/codegen/CodeGen.scala`
- `test/scala/amyc/test/CodegenTests.scala`

Tests and examples:

- `test/resources/parser/passing/HigherOrderFrontEnd.amy`
- `test/resources/parser/outputs/HigherOrderFrontEnd.amy`
- `test/resources/nameAnalyzer/passing/HigherOrderFrontEnd.amy`
- `test/resources/nameAnalyzer/outputs/HigherOrderFrontEnd.scala`
- `test/resources/typer/passing/HOFIdentity.amy`
- `test/resources/typer/passing/HOFHighOrder.amy`
- `test/resources/typer/passing/HOFSimple.amy`
- `test/resources/typer/failing/HOF*.amy`
- `test/resources/execution/passing/HOFCompose.amy`
- `test/resources/execution/passing/HOFMap.amy`
- `test/resources/execution/passing/HOFFold.amy`
- `test/resources/execution/outputs/HOFCompose.txt`
- `test/resources/execution/outputs/HOFMap.txt`
- `test/resources/execution/outputs/HOFFold.txt`
- `test/scala/amyc/test/ParserTests.scala`
- `test/scala/amyc/test/NameAnalyzerTests.scala`
- `test/scala/amyc/test/TypeTests.scala`
- `test/scala/amyc/test/InterpreterTests.scala`
- `test/scala/amyc/test/ExecutionTests.scala`

## Running the Compiler

The scripts are intended for macOS/Linux shells. On Windows, run them from WSL/Git Bash, or call the same `sbt` commands directly.

Interpret an Amy program:

```bash
./amyi.sh path/to/program.amy
```

Compile an Amy program to WebAssembly:

```bash
./amyc.sh path/to/program.amy
```

Type-check an Amy program:

```bash
./amytc.sh path/to/program.amy
```

## Running Tests

Prerequisites:

- Java runtime/JDK. Java 21 was used for testing.
- `sbt`
- `wasmtime` for WebAssembly execution tests

Run the full test suite:

```bash
sbt test
```

Run only code generation tests:

```bash
sbt "testOnly amyc.test.CodegenTests"
```

## Wasmtime Note

The repository includes wasmtime binaries under `bin/linux`, `bin/macos`, and `bin/windows`. The code generation tests first use `wasmtime` from `PATH` when available, then fall back to the bundled binary for the current OS.

If code generation tests fail because wasmtime cannot run, install wasmtime locally and make sure it is visible on `PATH`:

```bash
wasmtime --version
```

For example, on macOS:

```bash
brew install wasmtime
```

## Expected Result

The full test suite should pass:

```text
Passed: Total 224, Failed 0, Errors 0, Passed 224
```
