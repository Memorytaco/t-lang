# Codelayout for Compiler/

- Backend
    Where we put actual code for code generation, for now it has LLVM IR as one backend
- CodeGen
    Where we put helper functions for code generation
- Store
    Where we define staged for compiler

# Compiler stage

```text
┌─────────────┐
│SourceParsing│
└┬─┬──────────┘
 │┌▽────────────┐
 ││PreDesugaring│
 │└┬────────────┘
┌▽─▽─────────┐
│NameChecking│
└┬───────────┘
┌▽───────────┐
│TypeChecking│
└┬─┬─────────┘
 │┌▽─────────────┐
 ││PostDesugaring│
 │└┬─────────────┘
┌▽─▽───────────┐
│CoreGenerating│
└┬─────────────┘
┌▽────────────────┐
│Optimising       │
└┬───────────────┬┘
┌▽─────────────┐┌▽───────────┐
│ByteGenerating││MCGenerating│
└┬─────────────┘└┬───────────┘
┌▽───────────────▽┐
│Emiting          │
└─────────────────┘
```

- SourceParsing:
  - Fed with a source file and outputs AST for surface language.
  - It may or may not handle dependency between modules.
  - Operators are resolved in this stage and syntax tree is parsed as what it is. Compiler will lookup operator definition in parsed modules which contain surface language.
- PreDesugaring:
  - Fed with AST of surface language, and we add missing information for user typed type annotation. e.g. add flexible and rigid bindings: `forall a. (forall a. a -> a) -> a := forall a (b = forall a. a -> a). b -> a`
- NameChecking:
  - This is where we resolve all definition of symbols.
  - We make sure every symbol has its source and rename simple name to full qualified name. e.g. Let's say we have imported name `print` from module `std/io`, then we will rename parsed `Name "print"` to `QName "std/io" "print"`.
- TypeChecking:
  - This is the most difficult stage in compiler at now and there are still some technical issues not been solved.
    - How to represent type constraint in graph and combine it with MLF graphic type constraint
    - How to implement type application and keep information intact in graphic type constraint
  - This stage we kind check toplevel type definitions and then use the information to type check toplevel value definitions. Finally  we have a presolution for each type of expression and then pass this to next pass with type checked expression.
- PostDesugaring and CoreGenerating:
  - Where we have finally useful representation of AST and is the stage to generate core language called xMLF.
  - Every toplevel definition will be translated into this form to some extent.
  - Finally we can have this to generate code or to type check other codes.
- Optimising:
  - There is nothing to say about this.
  - This stage will be added after compiler is first released.
- ByteGenerating and MCGenerating:
  - We now use LLVM IR as MCGenerating target, so we can have targets to multiple platforms for free.
  - ByteGenerating is used for intermediate module interface where core language and type checked module is stored.
- Emiting:
  - All jobs are done, we invoke linker to emit executable or object.
