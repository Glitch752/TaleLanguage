# Tale, the programming language
This is a to-be-compiler interpreter for a custom language I made to learn Zig. The language features dynamic typing, first-class functions, object-oriented programming, and a simple standard library. It's powerful enough to build complex programs and abstractions, but high-level enough to be easy to use (with a few small caveats related to my decision to make it reference-counted).

I've been inspired by [Crafting Interpreters](https://craftinginterpreters.com/) and [Let's Build a Simple Interpreter](https://ruslanspivak.com/lsbasi-part1/), but I'm not following either of them exactly. The language currently consists of a relatively performant tree-walking interpreter, but I'm hoping to eventually implement a bytecode interpreter and a compiler. I'm also planning to eventually self-host the language.

<ol>
  <li>
    <a href="#demo-of-the-language">Demo of the language</a>
    <ul>
      <li><a href="#higher-order-functions-on-arrays">Higher-order functions on arrays</a></li>
      <li><a href="#donuttale">Donut.tale</a></li>
      <li><a href="#brainfuck">Brainfuck</a></li>
      <li><a href="#and-more">And more!</a></li>
    </ul>
  </li>
  <li><a href="#unit-tests">Unit tests</a></li>
  <li>
    <a href="#language-features">Language features</a>
    <ul>
      <li><a href="#important-note">Important note</a></li>
      <li><a href="#considerations-for-the-future">Considerations for the future</a></li>
    </ul>
  </li>
  <li><a href="#running">Running</a></li>
  <li><a href="#development">Development</a></li>
  <li><a href="#todo">TODO</a></li>
  <li>
    <a href="#random-questions">Random questions</a>
    <ul>
      <li><a href="#where-does-the-name-come-from">Where does the name come from?</a></li>
      <li><a href="#why-object-oriented">Why object-oriented?</a></li>
      <li><a href="#why-did-you-make-this">Why did you make this?</a></li>
      <li><a href="#whats-the-goal-of-the-language">What's the goal of the language?</a></li>
    </ul>
  </li>
</ol>

## Demo of the language

### Higher-order functions on arrays
This is a simple implementation of arrays with higher-order functions. The code is compacted to fit it in the README, but the original code is in `examples/working/higherOrderArrays.tale`.

```js
let Std = import("std");
let Object = class {};
let Array = class {
  constructor() { this.length = 0; this.inner = Object(); }
  push(value) {
    this.inner[Std.string(this.length)] = value; this.length = this.length + 1;
  }
  get(index) {
    if(index < 0 || index >= this.length) return null;
    return this.inner[Std.string(index)];
  }
  print() {
    Std.print("[");
    let i = 0; while(i < this.length) {
      Std.print(this.inner[Std.string(i)]);
      if(i < this.length - 1) Std.print(", ");
      i = i + 1;
    }
    Std.println("]");
  }
};
let MappableArray = class extending Array {
  map(f) {
    let result = MappableArray();
    let i = 0; while(i < this.length) { result.push(f(this.get(i))); i = i + 1; }
    return result;
  }
  filter(f) {
    let result = MappableArray();
    let i = 0; while(i < this.length) {
      let value = this.get(i);
      if(f(value)) result.push(value);
      i = i + 1;
    }
    return result;
  }
};

let array = MappableArray();
array.push(1); array.push(2); array.push(3); array.push(10); array.push(15);
Std.print("Original array: "); array.print();
let mappedArray = array.map(function(x) { return x * 2 + 1; });
let filteredArray = array.filter(function(x) { return x % 2 == 0; });
Std.print("Mapped array: "); mappedArray.print();
Std.print("Filtered array: "); filteredArray.print();
```

#### Output
```plaintext
Original array: [1, 2, 3, 10, 15]
Mapped array: [3, 5, 7, 21, 31]
Filtered array: [2, 10]
```

### Donut.tale
This is a port of `donut.c` to Tale. I'm still iterating on the language grammar, and intend to eventually add constructs like arrays; currently, I'm using an empty class instance as a dictionary. The code is compacted to fit it in the README, but the original code is in `examples/working/donut.tale`. On my machine, with optimizations enabled, this runs at about 11 FPS.
```js
let Std = import("std");
let tmr1 = null; let tmr2 = null; let A = 1; let B = 1;
let Empty = class{};
while(true) {
  A = A + 0.07; B = B + 0.03;
  let sinA = Std.sin(A); let cosA = Std.cos(A); let sinB = Std.sin(B); let cosB = Std.cos(B);
  let zBuffer = Empty(); let output = "";
  let k = 0; while(k < 1760) {
    zBuffer[Std.string(k)] = 0;
    if(k % 80 == 79) output = output + "\n";
    else output = output + " ";
    k = k + 1;
  }
  let j = 0;  while(j < 6.28) { // J is theta
    let jSin = Std.sin(j); let jCos = Std.cos(j);
    let i = 0; while(i < 6.28) { // I is phi
      let iSin = Std.sin(i); let iCos = Std.cos(i);
      let h = jCos + 2;
      let d = 1 / (iSin * h * sinA + jSin * cosA + 5);
      let t = iSin * h * cosA - jSin * sinA;
      let x = Std.floor(40 + 30 * d * (iCos * h * cosB - t * sinB));
      let y = Std.floor(12 + 15 * d * (iCos * h * sinB + t * cosB));
      let o = x + 80 * y; let strO = Std.string(o);
      if(y < 22 && y >= 0 && x >= 0 && d > zBuffer[strO]) {
        zBuffer[strO] = d;
        output =
          Std.substring(output, 0, o) +
          ".,-~:;=!*#$@"[Std.floor(Std.max(0,
              8 * ((jSin * sinA - iSin * jCos * cosA) * cosB - iSin * jCos * sinA - jSin * cosA - iCos * jCos * sinB)
          ))] +
          Std.substring(output, o + 1, Std.length(output));
      }
      i = i + 0.02;
    }
    j = j + 0.07;
  }
  Std.print(output);
}
```

#### Output (random frame, but it's animated)
```plaintext
                                   @@@@@@@@@@@$
                              $$$$$$#########$$$$$$
                            $####**!!!!!!!!!****##$$$$
                          ####**!!!!!!====!!!!!!**######
                        *##****!!==;;::::::;;==!*****####
                       ******!!==;:~-,,,,,--~:;;=!*****##*
                      !****!!==;:~,.........,-~:;=!!*******
                      !****!!=;:~-............-~:;=!!******!
                     ;!****!!=;:~,..        ..,~:;=!!*****!!
                     =!*****!!!;:-.          .,::==!!*****!=
                     ;!!******!!==:          -;==!!!*****!!=
                     :!!!*********!=        ;=!!!********!=;
                      ;=!***##########*!!*************!*!!;:
                      :=!!***##$$$$$@@@@$$$$$#####***!!!!=;
                       :;=!***###$$$@@@@@$$$$$###***!!!==;~
                        ~;=!*****##$$$$$$$$$###***!!!!=;:,
                         ,:;=!******#######******!!==;:~
                           ,~;;==!**!*********!!!=;;~-.
                              -~::;==========;;;:~-.
                                 .,--~~~~~~~--,.
```

## Brainfuck
This is a simple brainfuck interpreter (that doesn't support input) implemented in Tale. The code is compacted to fit it in the README, but the original code is in `examples/working/brainfuck.tale`. 
```js
let std = import("std");
let Memory = class {
    constructor() { this.length = 0; this.pointer = 0; }
    get() { if(this.pointer >= this.length) return 0; return this[std.string(this.pointer)]; }
    set(value) {
        if(this.pointer >= this.length) while(this.length <= this.pointer) {
            this[std.string(this.length)] = 0; this.length = this.length + 1;
        }
        this[std.string(this.pointer)] = value;
    }
    increment() { let next = this.get() + 1; if(next > 255) next = 0; this.set(next); }
    decrement() { let next = this.get() - 1; if(next < 0) next = 255; this.set(next); }
    forward() { this.pointer = this.pointer + 1; }
    backward() { this.pointer = this.pointer - 1; }
};
let interpret = function(program) {
    let output = ""; let memory = Memory();
    let i = 0; while(i < std.length(program)) {
        let command = program[i];
        if(command == ">") memory.forward();
        else if(command == "<") memory.backward();
        else if(command == "+") memory.increment();
        else if(command == "-") memory.decrement();
        else if(command == ".") output = output + std.charInt(memory.get());
        else if(command == "[") {
            if(memory.get() == 0) {
                let depth = 1; while(depth > 0) {
                    i = i + 1; if(i >= std.length(program)) return output;
                    if(program[i] == "[") depth = depth + 1;
                    else if(program[i] == "]") depth = depth - 1;
                }
            }
        } else if(command == "]") {
            if(memory.get() != 0) {
                let depth = 1; while(depth > 0) {
                    i = i - 1; if(i < 0) return output;
                    if(program[i] == "[") depth = depth - 1;
                    else if(program[i] == "]") depth = depth + 1;
                }
            }
        }
        i = i + 1;
    }

    return output;
};
// A brainfuck quine (program that prints itself); this loops back on itself a ton, so you pretty much need to run the program with ReleaseFast optimization for it to finish in a reasonable amount of time.
std.print(interpret("->++>+++>+>+>+++>>>>>>>>>>>>>>>>>>>>+>+>++>+++>++>>+++>+>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>+>+>>+++>>+++>>>>>+++>+>>>>>>>>>++>+++>+++>+>>+++>>>+++>+>++>+++>>>+>+>++>+++>+>+>>+++>>>>>>>+>+>>>+>+>++>+++>+++>+>>+++>>>+++>+>++>+++>++>>+>+>++>+++>+>+>>+++>>>>>+++>+>>>>>++>+++>+++>+>>+++>>>+++>+>+++>+>>+++>>+++>>++[[>>+[>]++>++[<]<-]>+[>]<+<+++[<]<+]>+[>]++++>++[[<++++++++++++++++>-]<+++++++++.<]"));
```

#### Output
```plaintext
->++>+++>+>+>+++>>>>>>>>>>>>>>>>>>>>+>+>++>+++>++>>+++>+>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>+>+>>+++>>+++>>>>>+++>+>>>>>>>>>++>+++>+++>+>>+++>>>+++>+>++>+++>>>+>+>++>+++>+>+>>+++>>>>>>>+>+>>>+>+>++>+++>+++>+>>+++>>>+++>+>++>+++>++>>+>+>++>+++>+>+>>+++>>>>>+++>+>>>>>++>+++>+++>+>>+++>>>+++>+>+++>+>>+++>>+++>>++[[>>+[>]++>++[<]<-]>+[>]<+<+++[<]<+]>+[>]++++>++[[<++++++++++++++++>-]<+++++++++.<]
```

### And more!

There are many other examples of the language in the `examples` directory. The `working` directory contains examples that are known to work, while the `testing` directory contains examples I want to eventually work, am currently working on fixing, or are known not to work.

## Unit tests
For even more examples of the language, check out the unit tests under `tests/`.
The `tests` directory contains tests for language features with a simple testing framework implemented in Tale itself!
To run the tests, use `taleExecutable tests/testFramework.tale` from the root directory.

## Language features
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
      - Access: `Point.x`, `Point["x"]`
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
    - [X] `parseInt` function: `Std.print(Std.parseInt("5"));` (Converts a string to an integer in base 10)
    - [X] `parseFloat` function: `Std.print(Std.parseFloat("5.5"));` (Converts a string to a float in base 10)
  - [X] `clock` function: `Std.print(Std.clock());` (Returns a calendar timestamp, in milliseconds, relative to UTC 1970-01-01)
  - [X] `panic` function: `Std.panic("Something went wrong!");` (Throws an irrecoverable error)
  - [X] `assert` function: `Std.assert(5 == 5);` (Panics if the condition is false)
  - [X] `random` submodule:
    - [X] `random.normalized`: `Std.print(Std.random.normalized());` (Returns a random number [0, 1))
    - [X] `random.seed`: `Std.random.seed(5);` (Seeds the random number generator, resetting the state)

### Important note
Since all memory in the language is reference-counted, reference cycles are possible (and easy to create). I'm considering switching to a garbage collector to avoid this issue, but for now, it's something to be aware of.
The following situations are examples of how reference cycles can be created:
- A class that holds a reference to itself
- A cycle of classes that hold references to each other
- A function that has a child function and the child function has more than 1 reference to the parent function (e.g. it's returned, stored in a variable, etc.)
This is quite limiting, but one of my other goals when making this language was to learn about reference counting, so I hope it's an understandable limitation.

### Considerations for the future
- Static typing
- Should we switch to a garbage collector to avoid reference cycles?
- How should we handle errors?
- Should statements be expressions? (E.g. `let x = if (true) { 5 } else { 3 }`)

## Running
I don't currently provide builds since I'm still developing the basic features and things are changing quickly. If you want to run the language as it currently is, you can do the following:
```sh
git clone https://github.com/Glitch752/TaleLanguage/
cd TaleLanguage
zig build --release=fast
./zig-out/bin/ZigCompiler ./examples/working/donut.tale
```
(And replace `.\examples\working\donut.tale` with the path to your source file)

## Development

Tale requires Zig version 0.13.0.
To run the program, use `zig run .\src\main.zig -freference-trace -- [tale file path]`. I plan to eventually migrate to Zig's build system, but this works well enough for now.

### Note
When developing on Windows and using Powershell, you'll need to run this because Powershell doesn't recognize UTF-8 by default:
```powershell
$OutputEncoding = [Console]::InputEncoding = [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding
```
I couldn't find the proper way to do this, but for now, this works for me.

## TODO
- [x] Implement a lexer
- [X] Implement a parser
- [X] Implement an interpreter
  - [X] Tree-walking interpreter
  - [ ] Bytecode interpreter
- [ ] Implement a compiler

## Random questions

### Where does the name come from?
A random noun generator. I needed a name and I couldn't think of one ¯\\\_(ツ)_/¯

### Why object-oriented?
It's the most familiar paradigm to me, and since I think it would be interesting to eventually self-host this language, I want to make it as easy as possible to work with.  
I also believe it can be a very powerful style when used correctly, and it's not that difficult to implement.  
Another part of it is that I want to learn how object-oriented features are implemented.  

### Why did you make this?
I'm making this language because I wanted to learn Zig and I'm interested in compiler design.

### What's the goal of the language?
To make a programming language that is relatively fast, easy-to-use, memory safe with reference counting, and (eventually) compilable to x86_64.