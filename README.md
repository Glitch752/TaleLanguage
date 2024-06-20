# Tale, the programming language
A work-in-progress programming language interpreter/compiler written in Zig.

Testing using `zig run .\src\main.zig -freference-trace -- ./examples/fizzbuzz.tale`, but I'll figure out how to use Zig's build system sooner or later.

# TODO
- [x] Implement a lexer
- [ ] Implement a parser
- [ ] Implement an interpreter
  - [ ] Tree-walking interpreter
  - [ ] Bytecode interpreter
- [ ] Implement a compiler

# FAQ (Questions to myself)

## Where does the name come from?
A random noun generator. I needed a name and I couldn't think of one ¯\\\_(ツ)_/¯

## Why did you make this?
I'm making this language because I wanted to learn Zig and I'm interested in compiler design.

## What's the goal of this project?
To make a programming language that is relatively fast, easy-to-use (ish), and compiles to various targets.