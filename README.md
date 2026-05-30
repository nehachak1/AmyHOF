# Functional Amy: Higher-Order Functions for the Amy Language

A compiler extension project for the Amy programming language that introduces first-class and higher-order functions. The project extends the complete compiler pipeline, including parsing, semantic analysis, interpretation, and WebAssembly code generation.

## Overview

Amy originally supported only top-level function definitions. Functions could not be passed as arguments, returned from other functions, stored in variables, or written anonymously.

This project extends Amy with:

- Function types
- Lambda expressions
- Higher-order functions
- Closures and lexical scoping
- Function-value application
- Type checking for function values
- Interpreter support
- WebAssembly backend support

The result is a fully functional higher-order programming model that works both in the interpreter and in generated WebAssembly.

---

## Features

### First-Class Functions

Functions can now:

- Be stored in variables
- Be passed as arguments
- Be returned from functions
- Be created anonymously

Example:

```amy
def makeAdder(n: Int(32)): Int(32) => Int(32) :=
  (x: Int(32)) => x + n
end makeAdder

makeAdder(5)(10)
```

---

### Anonymous Functions (Lambdas)

```amy
(x: Int(32)) => x + 1
```

Lambdas can be used wherever function values are expected.

---

### Higher-Order Functions

```amy
def map(f: Int(32) => Int(32), l: List): List := ...
```

Supports common functional programming patterns such as:

- map
- fold
- compose
- custom higher-order abstractions

---

### Closures

Captured variables remain available even after their defining scope exits.

```amy
def getMultiplier(factor: Int(32)): Int(32) => Int(32) :=
  (x: Int(32)) => x * factor
end getMultiplier
```

---

## Compiler Extensions

### Front End

Added new AST nodes:

```scala
Lambda(params, body)

Apply(fun, args)

FunctionType(args, ret)
```

Implemented:

- Lambda parsing
- Function type parsing
- Name analysis for function values
- Function application analysis
- Type inference and constraint generation

---

### Type Checker

Extended type checking to support:

- Function type compatibility
- Lambda inference
- Structural function type matching
- Recursive constraint solving

Example:

```scala
(Int => Int)
```

is treated as a first-class type.

---

### Interpreter

Added runtime closure support through:

```scala
FunctionValue(
  params,
  body,
  locals
)
```

Features:

- Lexical scoping
- Environment capture
- Dynamic function application

---

### WebAssembly Backend

Implemented closure conversion:

- Environment capture
- Closure allocation
- Function lifting
- Indirect function calls
- Runtime closure dispatch

This allows higher-order functions to execute correctly in generated WebAssembly code.

---

## Example Programs

### Returning Functions

```amy
def makeAdder(n: Int(32)): Int(32) => Int(32) :=
  (x: Int(32)) => x + n
end makeAdder

makeAdder(5)(10)
```

### Function Variables

```amy
val double = getMultiplier(2)

double(10)
```

### Map

```amy
map((x: Int(32)) => x + 1, list)
```

---

## Architecture

```text
Amy Source Code
        │
        ▼
      Lexer
        │
        ▼
      Parser
        │
        ▼
 Name Analysis
        │
        ▼
 Type Checker
        │
        ▼
 ┌──────────────┬──────────────┐
 │              │              │
 ▼              ▼              ▼
Interpreter   Closure      WebAssembly
              Conversion     Backend
```

---

## Key Concepts

- Compiler Construction
- Programming Language Design
- Functional Programming
- Higher-Order Functions
- Type Systems
- Lambda Calculus
- Closure Conversion
- Semantic Analysis
- WebAssembly
- Runtime Systems

---

## Technologies

- Scala
- Amy Compiler Framework
- WebAssembly (WASM)
- Functional Programming Concepts
- Compiler Design

---

## Authors

- Neha Chakraborty
- Cléa Maisonnier
- Martin Zimmer

---

## Academic Context

Developed as part of the **Computer Language Processing (CS-320) Extension Lab** at EPFL.

The project extends the Amy language with modern functional programming capabilities while preserving correctness across parsing, type checking, interpretation, and code generation.
