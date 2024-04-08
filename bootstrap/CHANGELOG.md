# Changelog for yo-lang bootstrap compiler

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

# Unreleased

## [0.2.0.0] - 2024-04-08

### Added

- Graph algorithms for new Graph.Data module
- More tests on Graph.Data

#### dependencies

### Removed

### Plan
- Refactor Parser for better error message and flexibility
- Refactor implementation of algebraic graph

### BugFix

# Release

## [0.1.0.1] - 2024-01-24

### Added

- Two recursion schemes in Recursion module

#### dependencies

- Add hand crafted Graph.Data module
- Prune all other unnecessary dependencies

### Removed

- Tlang directory
- Remove unnecessary nested deBruign type in Type.hs and replace it with a bifunctor

### Plan
- Refactor Parser for better error message and flexibility
- Refactor implementation of algebraic graph

### BugFix

- stabilize bumped compiler version and fix bugs caused by stack
  > Please see this issue [Incorrect caching breaks the build: Change in indirect dependency breaks the build#557]([text](https://github.com/commercialhaskell/stack/issues/5507)) for more info.

## [0.1.0.0] - 2024-01-01

### Added

Nothing

#### dependencies

- linear-base
  > used for supporting efficient implementation of algebraic graph

### Removed

- Tlang directory
- Remove unnecessary nested deBruign type in Type.hs and replace it with a bifunctor

### Plan
- Refactor Parser for better error message and flexibility
- Refactor implementation of algebraic graph
