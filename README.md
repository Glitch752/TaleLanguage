# Tale, the programming language
This is a to-be-compiler interpreter for a custom language I made to learn Zig.

I've been inspired by [Crafting Interpreters](https://craftinginterpreters.com/) and [Let's Build a Simple Interpreter](https://ruslanspivak.com/lsbasi-part1/).

# Demo of the language
This isn't a particularly good piece of code: it uses string manipulation hacks to work around the current lack of arrays, as well as temporary global functions I added. However, it demonstrates a relatively complicated program works in Tale--a port of `donut.c`. I'm still iterating on the language grammar, and intend to eventually add constructs like arrays. The code is compacted significantly to fit it in the README, but the original code is in `examples/working/donut.tale`.
```js
let Std = import("std");
let tmr1 = null; let tmr2 = null; let A = 1; let B = 1;
let asciiframe = function() {
    A = A + 0.07; B = B + 0.03;
    let sinA = Std.sin(A); let cosA = Std.cos(A); let sinB = Std.sin(B); let cosB = Std.cos(B);
    let output = ""; let zBuffer = "";
    let k = 0; while(k < 1760) {
        zBuffer = zBuffer + " ";
        if(k % 80 == 79) output = output + "\n";
        else output = output + " ";
        k = k + 1;
    }
    let j = 0; while(j < 6.28) { // J is theta
        let jSin = Std.sin(j); let jCos = Std.cos(j);
        let i = 0; while(i < 6.28) { // I is phi
            let iSin = Std.sin(i); let iCos = Std.cos(i);
            let h = jCos + 2; let d = 1 / (iSin * h * sinA + jSin * cosA + 5); let t = iSin * h * cosA - jSin * sinA;
            let x = Std.floor(40 + 30 * d * (iCos * h * cosB - t * sinB));
            let y = Std.floor(12 + 15 * d * (iCos * h * sinB + t * cosB));
            let o = x + 80 * y;
            let depthChar = Std.substring("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", Std.floor(d * 100), Std.floor(d * 100) + 1);
            if(y < 22 && y >= 0 && x >= 0 && x < 79 && Std.intChar(depthChar) > Std.intChar(Std.substring(zBuffer, o, o + 1))) {
                let N = 8 * ((jSin * sinA - iSin * jCos * cosA) * cosB - iSin * jCos * sinA - jSin * cosA - iCos * jCos * sinB);
                zBuffer = Std.substring(zBuffer, 0, o) + depthChar + Std.substring(zBuffer, o + 1, Std.length(zBuffer));
                let idx = 0;
                if(N > 0) idx = N;
                output = Std.substring(output, 0, o) + Std.substring(".,-~:;=!*#$@", idx, idx + 1) + Std.substring(output, o + 1, Std.length(output));
            }
            i = i + 0.02;
        }
        j = j + 0.07;
    }
    Std.print(output);
};
while(true) asciiframe();
```

There are many other examples of the language in the `examples` directory. The `working` directory contains examples that are known to work, while the `testing` directory contains examples I want to eventually work, am currently working on fixing, or are known not to work. `tests` also contains tests for the language, which display many features of the language.

# TODO
- [x] Implement a lexer
- [X] Implement a parser
- [X] Implement an interpreter
  - [X] Tree-walking interpreter
  - [ ] Bytecode interpreter
- [ ] Implement a compiler

# The language
The language, Tale, is not very well-defined yet -- I'm mostly experimenting with different ideas. However, here are the characteristics I'm currently implementing:
- [X] Dynamic typing (for now)
- [X] First-class functions
- [X] C-derived syntax
- [X] Automatic reference counting
- [X] Simple primatives: booleans, numbers, strings, functions, class types, class instances, modules (used for importing other files), and null.
- [X] Simple operators (no overloading):
  - [X] Arithmetic: `+`, `-`, `*`, `/`, `%` (modulo has the same precedence as multiplication and division)
  - [X] Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
  - [X] Logical: `!`, `&&`, `||`
    - [X] Truthiness: booleans are obvious, strings are truthy if they are not empty, numbers are truthy if they are not 0 or nan, and null is falsy
  - [X] Bitwise: `&`, `|`, `^`
    - We take the Javascript approach to bitwise operators: convert their operands to 32-bit signed integers before performing the operation
  - [X] Grouping: `()`
- [X] Simple control-flow constructs:
  - [X] `if` statements, along with `else if` and `else`
  - [X] `while` loops
  - [X] `for` loops (only for iterating over arrays -- `for (let x . array) { ... }`) (I'm not sure if I should add a more general `for` loop or even condense both loops into one)
  - [X] `break` and `continue` statements
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
- [X] Classes
  - [X] Declaration: `let Point = class { constructor(x, y) { this.x = x; this.y = y; } }`
    - The `constructor` method is called when the class is instantiated
  - [X] Instantiation: `let p = Point(5, 3);`
  - [X] Field access: `p.x`, `p.z = 5;` (creates a new field)
    - [X] Dynamic field access: `p["x"]` (I'm debating whether this is a good idea since it's not very type-safe, but it's a common feature in dynamic languages)
  - [X] Single inheritance: `let Point3D = class extending Point { constructor(x, y, z) { super.constructor(x, y); this.z = z; } }`
    - Constructors are inherited just like any other method
  - [X] Method access: `p.method()` (Just another function)
  - [X] Static:
    - [X] Methods: `let Point = class { static method() { return 5; } }`
      - Access: `Point.method()` (No `this` reference)
      - Static and instance methods can have the same name. You cannot call static methods on instances.
    - [X] Fields: `let Point = class {}; Point.x = 5;`
      - Access: `Point.x`
- [X] Imports from other files: `const test = import("test.tale");` (`test` is a module with all the exported values)
  - [X] Exporting values: `export let x = 5;`
  - [X] Exporting any type: `export let Point = class { constructor(x, y) { this.x = x; this.y = y; } }`
  - [X] Circular imports are handled gracefully
  - [X] Importing text files: `const text = import("text.txt");` (This is a string containing the file's contents)
  - [X] Imports can be made anywhere in the file. Side effects are executed immediately, but only occur once per file.
- [X] Simple standard library, accessible through the `std` module (Examples assume the file has `let Std = import("std");`):
  - [X] `print` and `println` functions: `Std.print("Hello, world!");`
  - [X] Math values:
    - [X] Trig functions (in radians): `sin`, `cos`, `tan`: `Std.print(Std.sin(3.14));`
    - [X] Inverse trig functions (in radians): `asin`, `acos`, `atan`: `Std.print(Std.atan(1));`
    - [X] `PI`, `E`, and `PHI` constants: `Std.print(Std.PI);`
    - [X] `INFINITY` and `NEGATIVE_INFINITY` constants: `Std.print(Std.INFINITY);`
    - [X] `floor`, `ceil`, and `round` functions: `Std.print(Std.floor(5.5));` (`round` rounds to the nearest integer, away from 0)
    - [X] `exp`, `exp2`, `log`, `log2`, and `log10` functions: `Std.print(Std.exp(5));`
    - [X] `pow` and `sqrt` functions: `Std.print(Std.sqrt(25));`
    - [X] `abs` function: `Std.print(Std.abs(-5));`
    - [X] `min` and `max` functions: `Std.print(Std.min(5, 3));`
  - [X] String functions:
    - [X] `length` function: `Std.print(Std.length("Hello, world!"));`
    - [X] `string` function: `Std.print(Std.string(5));` (converts any value to a string)
    - [X] `substring` function: `Std.print(Std.substring("Hello, world!", 0, 5));`
    - [X] `intChar` function: `Std.print(Std.intChar("A"));` (Converts a single-character string to its ASCII value)
    - [X] `charInt` function: `Std.print(Std.charInt(65));` (Converts an ASCII value to a single-character string)
  - [X] `clock` function: `Std.print(Std.clock());` (Returns a calendar timestamp, in milliseconds, relative to UTC 1970-01-01)
  - [X] `panic` function: `Std.panic("Something went wrong!");` (Throws an irrecoverable error)
  - [X] `assert` function: `Std.assert(5 == 5);` (Panics if the condition is false)

## Important note
Since all memory in the language is reference-counted, reference cycles are possible (and easy to create). I'm considering switching to a garbage collector to avoid this issue, but for now, it's something to be aware of.
The following situations are examples of how reference cycles can be created:
- A class that holds a reference to itself
- A cycle of classes that hold references to each other
- A function that has a child function and the child function has more than 1 reference to the parent function (e.g. it's returned, stored in a variable, etc.)
This is quite limiting, but one of my other goals when making this language was to learn about reference counting, so I hope it's an understandable limitation.

## Considerations for the future
- Static typing
- Should we switch to a garbage collector to avoid reference cycles?
- How should we handle errors?
- Should statements be expressions? (E.g. `let x = if (true) { 5 } else { 3 }`)

# Running
I don't currently provide builds since I'm still developing the basic features and things are changing quickly. If you want to run the language as it currently is, you can do the following:
```sh
git clone https://github.com/Glitch752/TaleLanguage/
cd TaleLanguage
zig build --release=fast
./zig-out/bin/ZigCompiler ./examples/working/donut.tale
```
(And replace `.\examples\working\donut.tale` with the path to your source file)

# Development

Tale requires Zig version 0.13.0.
To run the program, use `zig run .\src\main.zig -freference-trace -- [tale file path]`. I plan to eventually migrate to Zig's build system, but this works well enough for now.

## Random
When developing on Windows and using Powershell, you'll need to run this because Powershell doesn't recognize UTF-8 by default:
```powershell
$OutputEncoding = [Console]::InputEncoding = [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding
```
I couldn't find the proper way to do this, but for now, this works for me.

# Random questions

## Where does the name come from?
A random noun generator. I needed a name and I couldn't think of one ¯\\\_(ツ)_/¯

## Why object-oriented?
It's the most familiar paradigm to me, and since I think it would be interesting to eventually self-host this language, I want to make it as easy as possible to work with.  
I also believe it can be a very powerful style when used correctly, and it's not that difficult to implement.  
Another part of it is that I want to learn how object-oriented features are implemented.  

## Why did you make this?
I'm making this language because I wanted to learn Zig and I'm interested in compiler design.

## What's the goal of this project?
To make a programming language that is relatively fast, easy-to-use, memory safe with reference counting, and (eventually) compilable to x86_64.