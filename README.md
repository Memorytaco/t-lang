# Introduction

This small language aims to be a better c language with better type support and some
modern language features like **pattern match** and the real **sum type** or maybe **subtype**.

Yet, it is still under construction. You may not have a good experience or feel confused
at the time i writing down this README.

# How to play?

If you want to play with it, you need to know some basic knowledge with Haskell and have
a llvm 9 environment. That is because llvm-hs on hackage now (2022.10) depends on llvm 9 but
it could be updated in future since upstream repository now is working on it including
latest llvm 15 binding target.

This repository have the `bootstrap` directory holding source for compiler written in Haskell and That is
the only useful directory now. (We are at the very beginning time!)

switch into the bootstrap directory and using stack tool to build the project by `stack build`.
Then you are free to try `stack run` command.

# Future Plan

## Language Build Progress

- [ ] Support lazy evaluation
- [ ] Support pattern match
- [ ] Support lambda expression
- [ ] Support language module
- [ ] Support code branch selection

## Language Feature

- [x] Build up a robust simple typed system
    - [x] Build Constraint Based type infer system
- [ ] Build up unsafe communication scheme, to allow direct executable symbol map between t-lang and C
- [ ] Introduce Sum types
- [ ] Introduce Parametric type like system, more formal name later
- [ ] Introduce Adherence type like system, more formal name later
- [ ] Allow User defined operator

## Tools

- [ ] Build basic bootstrap compiler
- [ ] Allow the ability to export **Debug Symbol**

## General

- [ ] Build up **Object Model**
