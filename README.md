# Tale, the programming language
A work-in-progress programming language interpreter/compiler written in Zig.

Testing using `zig run .\src\main.zig -freference-trace -- ./examples/fizzbuzz.tale`, but I'll figure out how to use Zig's build system sooner or later.

I've been inspired by [Crafting Interpreters](https://craftinginterpreters.com/) and [Let's Build a Simple Interpreter](https://ruslanspivak.com/lsbasi-part1/).

# TODO
- [x] Implement a lexer
- [X] Implement a parser
- [X] Implement an interpreter
  - [X] Tree-walking interpreter
  - [ ] Bytecode interpreter
- [ ] Implement a compiler

# The language
The language, Tale, is not very well-defined yet -- I'm mostly experimenting with different ideas. However, here are the characteristics I'm aiming for:
- [X] Dynamic typing (for now)
- [ ] First-class functions
- [X] C-derived syntax
- [ ] Garbage collection
- [X] Simple primatives: Booleans, doubles, strings, functions, and null (I'm not sure if arrays or classes should go here)
- [X] Simple operators (no overloading):
  - [X] Arithmetic: `+`, `-`, `*`, `/`, `%` (modulo has the same precedence as multiplication and division)
  - [X] Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
  - [X] Logical: `!`, `&&`, `||`
    - [X] Truthiness: booleans are obvious, strings are truthy if they are not empty, numbers are truthy if they are not 0, and null is falsy
  - [X] Bitwise: `&`, `|`, `^`
    - We take the Javascript approach to bitwise operators: convert their operands to 32-bit signed integers before performing the operation
  - [X] Grouping: `()`
- [ ] Simple control-flow constructs:
  - [X] `if` statements, along with `else if` and `else`
  - [X] `while` loops
  - [ ] `for` loops (only for iterating over arrays -- `for (let x . array) { ... }`) (I'm not sure if I should add a more general `for` loop or even condense both loops into one)
  - [ ] `break` and `continue` statements
  - [X] `return` statements
- [X] Block scoping: `{}`
- [X] Comments: `//` and `/* */`
- [X] Variable declaration: `let x = 5;`, `let x;` (uninitialized -- value is `null`)
- [X] Assignment: `x = 5;`
- [X] Functions:
  - [X] Declaration: `let add = function(x, y) { return x + y; }` (This is a value that must be assigned to a variable)
    - If no value is returned, the function returns `null`
    - Functions hold a reference to the environment in which they were created -- commonly referred to as a closure
  - [X] Calling: `add(5, 3);` (`add` is a variable containing a function)
- [ ] Classes
  - [ ] Declaration: `let Point = class { let constructor = function(x, y) { this.x = x; this.y = y; } }`
    - The `constructor` method is called when the class is instantiated
  - [ ] Instantiation: `let p = Point(5, 3);`
  - [ ] Field access: `p.x`, `p.z = 5;` (creates a new field)
  - [ ] Single inheritance: `let Point3D = class extending Point { let constructor = function(x, y, z) { super.constructor(x, y); this.z = z; } }`
    - Constructors are inherited just like any other method
  - [ ] Method access: `p.method()` (Just another function)
  - [ ] Static:
    - [ ] Methods: `let Point = class { let static method() { return 5; } }`
      - Access: `Point.method()` (No `this` reference)
    - [ ] Fields: `let Point = class { let static x = 5; }`
      - Access: `Point.x`
- [ ] Extremely simple standard library, globally accessible as `Std` (for now):
  - [X] `print` function: `Std.print("Hello, world!");`
  - [ ] (From Crafting Interpreters) `clock` function: `std.print(Std.clock());` (Returns the time in seconds since the program started running)
  - [ ] `panic` function: `Std.panic("Something went wrong!");` (Throws an irrecoverable error)
  - [ ] `assert` function: `Std.assert(5 == 5);` (Panics if the condition is false)

## Considerations for the future
- Static typing
- How should imports work (and how should we change the standard library so it's not just a global object)?
- How should we handle errors?
- Should null be a type?
- Should statements be expressions? (E.g. `let x = if (true) { 5 } else { 3 }`)

# Development

## Random
When developing on Windows, you'll need to paste this in PowerShell because it doesn't recognize UTF-8 by default:
```powershell
$OutputEncoding = [Console]::InputEncoding = [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding
```
I might eventually find the proper way to do this, but for now, this works.

# FAQ (Questions to myself)

## Where does the name come from?
A random noun generator. I needed a name and I couldn't think of one ¯\\\_(ツ)_/¯

## Why object-oriented?
It's the most familiar paradigm to me, and since I want to eventually self-host this language, I want to make it as easy as possible to work with.  
I also personally think it can be a very powerful style when used correctly, and it's not that difficult to implement.  
Another part of it is that I want to learn how object-oriented features are implemented.  

## Why did you make this?
I'm making this language because I wanted to learn Zig and I'm interested in compiler design.

## What's the goal of this project?
To make a programming language that is relatively fast, easy-to-use (ish), and compiles to various targets.