# Ocaml-Language-Interpreter
Ocaml Interpreter that read commands from an input file executes them sequentially, and writes results to an output file. The interpreter processes stack operations, arithmetic, and boolean expressions according to a defined grammar.
Part 1: Basic Computation

Stack Operations

push <int|string|name|:true:|:false:|:error:|:unit:> – Push values onto the stack
pop – Remove top element
swap – Swap top two elements

Arithmetic (integers only)

add, sub, mul, div, rem – Binary operations on top two values
neg – Unary negation
Errors occur on invalid types, insufficient elements, or divide/mod by zero

Type & Output

toString – Convert top value to string
println – Print top string to output file

Program Control

quit – Terminate interpreter

Error Handling

Any invalid operation pushes :error: and restores popped values
Part 2: Variables & Scope

String & Boolean Ops

cat – Concatenate two strings
and, or, not – Boolean logic

Comparisons

equal, lessThan – Integer comparisons

Variables

bind – Bind names to values (stored in environment)
Supports integers, strings, booleans, :unit:, and bound values

Control Flow

if – Conditional (uses boolean to select between two values)

Scoping

let ... end – Create scoped environments with nested bindings
Part 3: Functions

Function Definitions

fun <name> <arg> ... funEnd – Define functions
Creates closures (function body + environment + argument)
Pushes :unit: and stores function in environment
