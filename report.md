# Improving Amy Language Features

**Computer Language Processing 2026 Extension Lab Final Report**

## Contact Information

**Neha Chakraborty**  
Email: neha.chakraborty@epfl.ch

**Cléa Maisonnier**  
Email: clea.maisonnier@epfl.ch

**Martin Zimmer**  
Email: martin.zimmer@epfl.ch

## Abstract

Our project extends Amy with higher-order functions. The goal was to allow functions to be used as values: they can be passed as arguments, returned from other functions, stored in variables, and written anonymously. This makes programs such as `map`, `fold`, and `compose` possible in Amy. We implemented the feature in three parts. The front end was extended with function types, lambda expressions, function-value application, name analysis, and type checking. The interpreter was updated to represent lambdas as function values with captured environments. The WebAssembly backend was extended with closure conversion and dynamic dispatch for compiled function values. The result is that higher-order programs now work both in the interpreter and in generated WebAssembly.

## Introduction

In the non-optional labs, we built the main Amy compiler pipeline. The source program is first lexed into tokens, then parsed into an AST. Name analysis resolves identifiers, and type checking verifies that expressions are used with consistent types. After these phases, Amy programs can either be interpreted directly or compiled to WebAssembly.

Our project extends this pipeline with higher-order functions. Before the extension, Amy functions could only be called by name as top-level definitions. They could not be passed as arguments, returned from other functions, or stored in variables. This made common functional patterns such as `map`, `fold`, and `compose` hard to write naturally.

We split the implementation into three parts:

- **Front end:** added function types, lambda expressions, function-value application, name analysis, and type checking.
- **Interpreter:** represented lambdas as function values with their captured environment.
- **WebAssembly backend:** compiled closures by storing captured variables and dispatching calls to lifted lambda functions.

This follows the existing compiler architecture: the front end defines which programs are valid, the interpreter defines their runtime behavior, and the backend makes the same behavior available in compiled WebAssembly.

## Examples and Use of the Extensions

### Using the Front-End Extension

Our extension lets Amy users write functions as values. A function can now be passed as an argument, returned from another function, stored in a variable, and written anonymously.

A basic use case is a function that returns another function:

```amy
object HigherOrderFrontEnd
  def makeAdder(n: Int(32)): Int(32) => Int(32) :=
    (x: Int(32)) => x + n
  end makeAdder

  makeAdder(5)(10)
end HigherOrderFrontEnd
```

Here `makeAdder(5)` returns a function, and the second call `(10)` applies it. The lambda uses `x`, its own parameter, and `n`, which comes from the outer function. This is one of the important cases for the extension, because the compiler must accept the syntax, resolve both variables correctly, and check that the returned function has type `Int(32) => Int(32)`.

Users can also store function values in variables:

```amy
object HOFHighOrder
  def getMultiplier(factor: Int(32)): Int(32) => Int(32) :=
    (x: Int(32)) => x * factor
  end getMultiplier

  val double: Int(32) => Int(32) = getMultiplier(2);

  double(10)
end HOFHighOrder
```

This allows users to build specialized functions from more general ones. In this example, `double` is a function value. Calling `double(10)` is valid because `double` has type `Int(32) => Int(32)`.

The extension is especially useful for reusable functions such as `map`:

```amy
object HOFMap
  def map(f: Int(32) => Int(32), l: L.List): L.List :=
    l match {
      case L.Nil() => L.Nil()
      case L.Cons(h, t) => L.Cons(f(h), map(f, t))
    }
  end map

  val list: L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));

  map((x: Int(32)) => x + 1, list)
end HOFMap
```

Here the user passes an anonymous function directly to `map`. This avoids writing a separate named function for a small operation. The compiler checks that the argument passed to `map` really has type `Int(32) => Int(32)`.

We also considered invalid uses. For example:

```amy
object BadCall
  val x: Int(32) = 5;
  val y: Int(32) = x(10)
end BadCall
```

This program is rejected because `x` is an integer, not a function. Another invalid case is:

```amy
object BadLambda
  val f: Int(32) => Int(32) = (x: Int(32)) => true
end BadLambda
```

This is rejected because the lambda body returns `Boolean`, while the declared function type expects `Int(32)`.

Users do not need any new command-line option. Higher-order functions are part of the normal Amy language after the extension. The usual commands still apply:

```bash
./amyi.sh program.amy
./amyc.sh program.amy
```

## Implementation

### Theoretical Background

To implement the higher-order functions extension on our Amy Language Compiler, we needed to update our compiler’s entire pipeline, including lexer, parser, name analyzer, type checker, interpreter and code generator. Indeed we have to add lambdas and function types meaning changing how we interpret functions all along the different implementation levels.

#### Front-End Parser: Syntax Ambiguities and LL(1) Constraints

An LL(1) parser reads input left-to-right and decides which rule to apply using only one token of lookahead [lec04a, lec05a]. This means the grammar must be free of any choice ambiguity.

When we add lambdas, we introduce a conflict with unit literals, because both begin with a left parenthesis. From the first token alone, the parser cannot know whether it starts a lambda, such as `(x: Int(32)) => x + 1`, or the unit literal `()`.

To preserve the LL(1) property and eliminate choice ambiguity, we remove this ambiguity before the parser sees the tokens [lec05a].

#### Type Checker: First-Class Functions and Structural Inference

In our updated compiler, we must now consider our functions as first-class citizens [Schinz 2020a], meaning we can pass them as parameters and return them from other definitions.

To keep soundness across types [lec05c], the type checker has to recursively go through each function to check that the parameters and return types match.

#### Backend: The Closure Problem and Closure Conversion

Higher-order functions may capture variables from their defining scope, so a plain code pointer is not enough to represent them at runtime. Closure conversion solves this by representing every non-closed function as a closure: a code pointer paired with an environment storing its free variables [Schinz 2020a]. Creating a lambda allocates this environment; calling it extracts the code pointer and passes the environment as an extra argument.

This transformation also affects the backend: closures must be placed in memory in a consistent layout, and the code generator must call them indirectly by first loading the function’s code pointer from that layout. After closure conversion, every function becomes a closed value that can be safely invoked even after its defining scope is gone.

## Implementation Details

### Front End

The main difficulty in the front end was that function calls became ambiguous. Before the extension, a call such as `foo(1)` could only mean a call to a named function or constructor. After adding higher-order functions, the called expression can also be a local function value, for example a parameter `f`, or the result of another call such as `makeAdder(5)(10)`.

To keep this distinction clear, we added separate AST nodes:

```scala
case class Lambda(params: List[ParamDef], body: Expr) extends Expr
case class Apply(fun: Expr, args: List[Expr]) extends Expr
case class FunctionType(args: List[Type], ret: Type) extends Type
```

`Call` is still used for known top-level functions and constructors. `Apply` is used for calling an expression that has a function type. This avoided mixing two different cases in the same tree node.

A non-obvious parsing problem was lambda syntax. A lambda starts with a parenthesis:

```amy
(x: Int(32)) => x + 1
```

but so do normal parenthesized expressions. Since the parser is LL(1), it cannot decide from the first token alone which case it is seeing. Our solution was to preprocess the token stream and replace only lambda-opening parentheses with a special `LambdaOpenToken`. The parser can then recognize lambdas without breaking normal parentheses or unit patterns. This preprocessing step also preserves the LL(1) property of the grammar. After the rewrite, lambda expressions and ordinary parenthesized expressions no longer start with the same token: lambdas start with `LambdaOpenToken`, while normal parentheses still start with `(`. As a result, the parser can choose the correct production using only one token of lookahead, and `Parser.program.isLL1` succeeds without reporting conflicts.

Name analysis also needed special handling. The parser first represents an unqualified call as an application:

```amy
foo(1)
```

becomes conceptually:

```amy
Apply(Variable(foo), List(1))
```

This design also avoids an LL(1) conflict between variables and unqualified calls. Instead of parsing them with two competing productions, both cases are handled by the same `variableOrCall` rule, and the distinction is resolved later during name analysis. During name analysis, we rewrite this to a normal `Call` only if `foo` is a known global function or constructor and is not shadowed by a local binding. Otherwise, it remains an `Apply`.

This is important for:

```amy
def applyTwice(f: Int(32) => Int(32), x: Int(32)): Int(32) :=
  f(f(x))
end applyTwice
```

Here `f` is a parameter, so `f(x)` must be kept as a function-value application.

For type checking, the key rule is that the left side of an `Apply` must have a function type matching the arguments. We create fresh type variables for the argument and return types, then generate constraints:

```scala
case Apply(fun, args) =>
  val retType = TypeVariable.fresh()
  val argTypes = args.map(_ => TypeVariable.fresh())

  genConstraints(fun, FunctionType(argTypes, retType)) ++
  args.zip(argTypes).flatMap {
    case (arg, expected) => genConstraints(arg, expected)
  } ++
  topLevelConstraint(retType)
```

This catches the main corner cases: calling a non-function, passing arguments of the wrong type, and assigning a lambda to an incompatible function type. The hard part was not the lambda node itself, but making lambdas and function-value calls coexist with Amy’s existing named calls and scoping rules.

### Semantic Analysis and Runtime Evaluation

#### Type Checker Extensions

The type checker is the step in the compiler’s pipeline that ensures that the program maintains coherent types. Originally, it checked that the types of the given arguments matched the function’s parameter types, and that the body of the function evaluated the expected return type.

With our extension, our Amy Language Compiler now supports higher-order functions. This means that functions can now be passed to other functions as arguments and can return function types, making functions first-class citizens just like integers or booleans.

This means that the way we generate constraints must be updated to allow for these changes. Checking if two function arguments match is not as straightforward as it is for integers or booleans. Because functions contain parameters and a body, the type checker now has to recursively go through each parameter and evaluate the return type.

To do so, we updated the `TypeChecker.scala` file with four new features.

##### Case `Lambda`

```scala
case Lambda(params, body) =>
  val parameter_types = params.map(_.tt.tpe)
  val lambda_env = params.map { p => p.name -> p.tt.tpe }.toMap
  val tv = TypeVariable.fresh()
  genConstraints(body, tv)(using env ++ lambda_env) ++
    topLevelConstraint(FunctionType(parameter_types, tv))
```

We introduce a new AST case to represent anonymous function definitions, which include their parameters and body expression. To check the types of a lambda, we first map each parameter name to its type. We then create a fresh `TypeVariable` which will contain the return type of the function’s body. Finally, we constrain the lambda node to a `FunctionType(params, tv)`, which links the current lambda node representation to its formal function type representation.

##### Case `Apply`

```scala
case Apply(fun, args) =>
  val funTv = TypeVariable.fresh()
  val argsTv = args.map(_ => TypeVariable.fresh())
  val funArgsGenConstr = genConstraints(fun, FunctionType(argsTv, funTv))
  val argsGenConstr = args.zip(argsTv).flatMap {
    case (arg, retArg) => genConstraints(arg, retArg)
  }
  funArgsGenConstr ++
    argsGenConstr ++
    topLevelConstraint(funTv)
```

We introduce a new AST case to check dynamic function calls so that the higher-order function expression is evaluated to a function type. We implemented it by first generating a fresh `TypeVariable` for every argument so that their types can each be checked independently. We then generate constraints to check that the function expression matches the `FunctionType`, to verify that we have the right number of arguments and the expected types. Finally, we examine whether the return type of the function matches the expected one before executing it.

##### `solveConstraints`

```scala
case (FunctionType(args1, ret1), FunctionType(args2, ret2)) =>
  if (args1.size != args2.size) {
    ctx.reporter.error(
      s"Both function types do not match, they have a different number of arguments : found -> ${args1.size} arguments, expect -> ${args2.size} arguments"
    )
  } else {
    val constraintsArgs = args1.zip(args2).map {
      case (arg1, arg2) => Constraint(arg1, arg2, pos)
    }
    val constraintRet = Constraint(ret1, ret2, pos)
    solveConstraints(constraintsArgs ++ (constraintRet :: more))
  }
```

We added a structural matching rule to solve constraints when encountering two function types. We first check if they have the same number of arguments, and if so, create constraints between each corresponding pair of arguments from the found and expected functions. We then create another constraint for the return types, and finally prepend them all to the remaining constraint queue to be solved sequentially.

##### `subst` Extension

```scala
def subst(tpe: Type, from: Int, to: Type): Type = {
  tpe match {
    case TypeVariable(`from`) => to
    case FunctionType(args, ret) =>
      FunctionType(args.map(arg => subst(arg, from, to)), subst(ret, from, to))
    case other => other
  }
}
```

We finally had to extend our `subst` function, whose role is to substitute every occurrence of a given type variable, to handle `FunctionType` structures. Since we are dealing with functions, we need to recursively apply the substitution rules through all parameter types and the return type for every `FunctionType`.

#### Interpreter Extensions

Originally, the interpreter evaluated each node of the AST sequentially, handling simple values like integers and booleans alongside standard arithmetic operations. However, now that we support higher-order functions, the interpreter must understand the new format of anonymous functions (`Lambda`), interpret them into a `FunctionValue`, and then execute them using a new `Apply` case.

To do so, we updated the `Interpreter.scala` file with three new features.

##### Case Class `FunctionValue`

```scala
case class FunctionValue(
  params: List[Identifier],
  body: Expr,
  locals: Map[Identifier, Value]
) extends Value
```

We define a new runtime value to represent functions as first-class citizens. This new representation of function values acts as a lexical closure, storing the list of parameters, the function’s body expression, as well as a snapshot of the current local variables environment (`locals`).

##### Case `Lambda`

```scala
case Lambda(params, body) =>
  FunctionValue(params.map(p => p.name), body, locals)
```

When the interpreter traverses the AST and encounters a lambda node, it packages it into a `FunctionValue` where it stores each parameter identifier, the lambda’s function body, and a snapshot of the current locals.

##### Case `Apply`

```scala
case Apply(fun, args) =>
  val funValue = interpret(fun) match {
    case funct: FunctionValue => funct
    case _ =>
      ctx.reporter.fatal(s"Error: the type isn't a FunctionValue, we go instead : $fun")
  }
  val interpArgs = args.map(arg => interpret(arg))
  if (funValue.params.size != interpArgs.size) {
    ctx.reporter.fatal(
      s"Error: different number of arguments : fun -> ${funValue.params.size} arguments vs args -> ${interpArgs.size} arguments "
    )
  }

  val newLocals: Map[Identifier, Value] =
    funValue.locals ++ funValue.params.zip(interpArgs).toMap
  interpret(funValue.body)(using newLocals)
```

When a function value needs to be executed, the interpreter cannot use the existing `Call` node since that is strictly reserved for static functions defined globally at compile-time. Therefore, we implement `case Apply` to handle the evaluation of dynamic function values. It first evaluates the function expression to retrieve the target closure, reporting a fatal error if the value is not a valid `FunctionValue`. It then interprets each argument expression and verifies that the number of provided arguments matches the number of parameters the function expects. Finally, it evaluates the function's body within a newly synthesized environment, where the captured locals from the closure are updated with the incoming argument bindings.

## Possible Extensions and Conclusion

## References

- [Schinz 2020a] Michel Schinz: Closure conversion. Advanced Compiler Construction, EPFL, 2020. <https://cs420.epfl.ch/archive/20/c/05_cc.html>
- [Schinz 2020b] Michel Schinz: Closure conversion, or: values representation for functions. Advanced Compiler Construction, EPFL, lecture slides, 2020. <https://cs420.epfl.ch/archive/20/s/acc20_05_cc.pdf>
