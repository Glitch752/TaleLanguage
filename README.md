# Tale, the programming language
This is a to-be-compiler interpreter for a custom language I made to learn Zig.

I've been inspired by [Crafting Interpreters](https://craftinginterpreters.com/) and [Let's Build a Simple Interpreter](https://ruslanspivak.com/lsbasi-part1/).

# Demo of the language
This isn't a particularly good piece of code: it uses string manipulation hacks to work around the current lack of arrays, as well as temporary global functions I added. However, it demonstrates a relatively complicated program works in Tale--a port of `donut.c`. I'm still iterating on the language grammar, and intend to eventually add constructs like arrays.
```
let tmr1 = null;
let tmr2 = null;
let A = 1;
let B = 1;

let asciiframe = function() {
    A = A + 0.07;
    B = B + 0.03;

    let sinA = sin(A);
    let cosA = cos(A);
    let sinB = sin(B);
    let cosB = cos(B);

    let output = "";
    let zBuffer = "";

    let k = 0;
    while(k < 1760) {
        zBuffer = zBuffer + " ";
        if(k % 80 == 79) {
            output = output + "\n";
        } else {
            output = output + " ";
        }
        k = k + 1;
    }

    let j = 0; // J is theta
    while(j < 6.28) {
        let jSin = sin(j);
        let jCos = cos(j);

        let i = 0; // I is phi
        while(i < 6.28) {
            let iSin = sin(i);
            let iCos = cos(i);

            let h = jCos + 2;
            let d = 1 / (iSin * h * sinA + jSin * cosA + 5);
            let t = iSin * h * cosA - jSin * sinA;

            let x = floor(40 + 30 * d * (iCos * h * cosB - t * sinB));
            let y = floor(12 + 15 * d * (iCos * h * sinB + t * cosB));

            let o = x + 80 * y;

            let depthChar = substring("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", floor(d * 100), floor(d * 100) + 1);
            if(y < 22 && y >= 0 && x >= 0 && x < 79 && intChar(depthChar) > intChar(substring(zBuffer, o, o + 1))) {
                let N = 8 * ((jSin * sinA - iSin * jCos * cosA) * cosB - iSin * jCos * sinA - jSin * cosA - iCos * jCos * sinB);

                zBuffer = substring(zBuffer, 0, o) + depthChar + substring(zBuffer, o + 1, length(zBuffer));
                let idx = 0;
                if(N > 0) idx = N;

                output = substring(output, 0, o) + substring(".,-~:;=!*#$@", idx, idx + 1) + substring(output, o + 1, length(output));
            }

            i = i + 0.02;
        }
        j = j + 0.07;
    }

    print(output);
};

while(true) {
    asciiframe();
}
```

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
- [X] Simple primatives: Booleans, doubles, strings, functions, and null (I'm not sure if arrays or classes should go here)
- [X] Simple operators (no overloading):
  - [X] Arithmetic: `+`, `-`, `*`, `/`, `%` (modulo has the same precedence as multiplication and division)
  - [X] Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
  - [X] Logical: `!`, `&&`, `||`
    - [X] Truthiness: booleans are obvious, strings are truthy if they are not empty, numbers are truthy if they are not 0, and null is falsy
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
  - [X] Single inheritance: `let Point3D = class extending Point { constructor(x, y, z) { super.constructor(x, y); this.z = z; } }`
    - Constructors are inherited just like any other method
  - [X] Method access: `p.method()` (Just another function)
  - [ ] Static:
    - [ ] Methods: `let Point = class { static method() { return 5; } }`
      - Access: `Point.method()` (No `this` reference)
    - [ ] Fields: `let Point = class { static x = 5; }`
      - Access: `Point.x`
- [ ] Simple standard library, ~~globally accessible as `Std` (until I implement imports)~~ globally accessible for now:
  - [X] `print` function: `print("Hello, world!");`
  - [ ] `clock` function: `print(clock());` (Returns the time in seconds since the program started running)
  - [ ] `panic` function: `panic("Something went wrong!");` (Throws an irrecoverable error)
  - [ ] `assert` function: `assert(5 == 5);` (Panics if the condition is false)

## Considerations for the future
- Static typing
- How should imports work (and how should we change the standard library so it's not just a global object)?
- How should we handle errors?
- Should statements be expressions? (E.g. `let x = if (true) { 5 } else { 3 }`)

# Running
I don't currently provide builds since I'm still developing the basic features and things are changing quickly. If you want to run the language as it currently is, you can do the following:
```sh
git clone https://github.com/Glitch752/TaleLanguage/
cd TaleLanguage
zig build -Doptimize=ReleaseFast
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
