# Introduction

The language is targeted at System level with zero abstraction cost, equipped with better type support and some
modern language features like **pattern match** and **sum type**.

Yet, it is still under early construction. If you have any interest in this, please refer to [BrokenExperiment](https://github.com/Memorytaco/t-lang/tree/BrokenExperiment)
branch for latest update.

# How to play?

If you want to play with it, you need to know some basic knowledge with Haskell and have
a llvm 9 environment (llvm-config should be available in PATH). That is because
llvm-hs on hackage now (2023.2) depends on llvm 9 but
it could be updated in future since upstream repository now is working on it including
latest llvm 15 binding target.

This repository has the `bootstrap` directory (which is the only meaningful directory now) holding source
for compiler written in Haskell. (We are at the very beginning!)

switch into the bootstrap directory and using stack tool to build the project by `stack build`.
Then you are free to try `stack run` command.

> You can always get a successful running on the `main` branch and have quite a chance to see
> Haskell error on the latest branch but you can always play with the "good" part of the code
> and thanks for haskell's REPL.