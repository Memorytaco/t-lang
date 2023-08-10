{- | a helper module which provides many common codegen pattern with
-- carefully designed environment.
--
-}
module Compiler.Backend.LLVM.Runtime
  (
    wrapMain
  )
where

import Compiler.Backend.LLVM.Definition
import Compiler.Backend.LLVM.IR (ensureNamedBlock, ensureBlockEndWith)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import qualified LLVM.AST.Constant as LLVM
import Data.String (IsString)

-- | make a main function as execution environment for generated code.
wrapMain :: (MonadLLVMBuilder m, IsString name) => ([(name, (LLVM.Type, LLVM.Operand))] -> m LLVM.Operand) -> m LLVM.Operand
wrapMain make = globalFunction "main" [(LLVM.i32, Just "argc"), (LLVM.ptr, Just "argv")] LLVM.i32 \case
  [argc, argv] -> do
    ensureNamedBlock "main.start" -- create start block
    r <- make [("argc", (LLVM.i32, argc)), ("argv", (LLVM.ptr, argv))]
    -- create ending block
    LLVM.typeOf r >>= \case
      Right (LLVM.IntegerType 32) -> ensureBlockEndWith (LLVM.Do $ LLVM.Ret (Just r) [])
      _ -> ensureBlockEndWith . LLVM.Do $ LLVM.Ret (Just $ LLVM.ConstantOperand $ LLVM.Int 32 0) []
  _ -> error "impossible when building wrapper for main function"
