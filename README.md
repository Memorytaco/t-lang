# What is this?

The language is targeted at System Level with zero abstraction cost, equipped with better type support and some modern language features like **pattern match** and **sum type**.

Yet, it is still under early construction. If you have any interest in this, please refer to "What to contribute?" section.

# How to play?

If you want to play with it, you need to know some basic knowledge with Haskell and have a llvm 15 environment (`llvm-config` should be available in PATH).

This repository has the `bootstrap` directory (which is the only meaningful directory for now) holding source for compiler written in Haskell. (We are at the very beginning!)

switch into the bootstrap directory and using stack tool to build the project by `stack build`.
Then you are free to try `stack run repl` command, which brings up an interpreter with following commands:

1. `:dump` : to show generated llvm ir code for expression.
2. `:def` : to define global structures like a type declaration or value definition or FFI.
3. `:load` : to load a source file and parse it, then print raw AST structure.
4. `:list` : list definitions available for current repl session after `:def` command.
5. `:list module` : to show modules available in scope. The names can be used for command `:showm`.
6. `:list source` : to print original source file names for modules.
7. `:type` : to query a type of an expression
8. `:source` : to print original source file content for a module.
9. `:showm` : to show contents of a module.
10. it defaults to print out parsed AST of your typing.

> You are guaranteed to get a successful running on the `main` branch.

# What to contribute?

## General introduction

This is something still under consideration, But here are some topics you may pick up at your interest:

1. Type system

   This language combines two systems:

   - `SystemFC` for a running system (it lacks constraint support for now)

   - `MLF` as core type language

   These two systems are verified already, so what left to us are simply implementation details. (It may still need a formal presentation combining these two systems which will be done in future)

   We do simple first order unification to infer kinds of types and use graph based unification for MLF types with some straightforward extensions from SystemFC. Type level abstraction are restricted to injective ones and this should not worry us in the near future.

   This is the common setting and for now it is all we care about.

   > For some reason, constraint system (type class) is not presented and this is our goal.

2. The framework

   After months' work, a very basic framework has been setup, using `tagless final style` and [*Data types a la carte*](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409) . All operations are designed around two or three  extensible structures and all transformation will have a "fold"-like shape with arbitrary extensions to model effects.

   We have concept as "pass" to do transformation between AST (this is not in active development right now) and this transformation is defined as "generic" fold using type class.

   You need to be familiar with [recursion-scheme](https://hackage.haskell.org/package/recursion-schemes) to understand most cases, because recursion is everywhere. With help of monad and type class, this seems to be suprisingly powerful and yet flexible.

   A general pipeline of compiler is:

   ```dot
   pipeline {
     Program (Parser) -> AST -> Semantic AST (Inference) -> xMLF -> ANF -> LLVM IR -> MC
   }
   ```

3. The lowlevel part

   This is something still under investigation. It relates to code generation and runtime model of this language. It now has nearly no progress but some open problems need to be solved:

   1. How to design type directed closure conversion pass in ANF?

      > potential solution: trampoline as saver

   2. What runtime representation of recursive type should be

      > potential solution: introduce linear type to do count and use boxed value

   3. How to design type directed optimization procedure in ANF?

   4. What is intermediate representation of a module?

      > potential solution: mangled name for module definition for representing namespace and intermediate representation for analysed module with ability to be converted into object file

   5. Is region inference a saver to generate cost free code?

## Code structure

Please read README in `bootstrap/src/Compiler` for a general introduction of compiler pipeline and relative code layout.

## Roadmap

- [ ] [2023.09.10] First release of a very basic prototype compiler.

  - With JIT available for very limited functionality.

  - Allow type inference for expression in REPL but with no compilation.

  - Only MLF type system is provided, and it has no kinds.

- [ ] [2023.12.10] Lift MLF to MLFω and add kind support.

# Principles

We have following principles, and encourage introducing new breaking changes as long as it gets equivalent value.

- Intuition comes first

- Get it done and verify it later

- Everything is under bargining
