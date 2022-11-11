module Tlang.Codegen.Core
  ( CodegenT (..)
  , LLVM (..)
  , runCodegen
  , runLLVM
  , buildLLVM
  )
where

{-

Basic primitives for building up LLVM IR. No code generation logic here.

Lack large amount of basic primitives here or lack one translation layer to map high level
operation into assembly like instructions.

-}

import LLVM.AST
import LLVM.IRBuilder

import Control.Monad.RWS
import Control.Monad.Except (MonadError (..))

newtype CodegenT r s e m a = CodegenT { runCodegenT :: IRBuilderT m a }
  deriving (Functor, Applicative, Monad, MonadIRBuilder, MonadError e, MonadState s, MonadReader r)

newtype LLVM s e m a = LLVM (ModuleBuilderT m a)
  deriving (Functor, Applicative, Monad, MonadModuleBuilder, MonadTrans, MonadError e, MonadState s, MonadFail)

runCodegen :: Monad m
           => CodegenT r s e m a -> IRBuilderState
           -> m (a, [BasicBlock])
runCodegen m s = runIRBuilderT s $ runCodegenT m

runLLVM :: MonadError e m => LLVM s e m a -> ModuleBuilderState -> m (a, [Definition])
runLLVM (LLVM m) = flip runModuleBuilderT m

buildLLVM :: MonadError e m => Module -> ModuleBuilderState -> LLVM s e m a -> m (a, Module)
buildLLVM mod s ma = do
  (a, defs) <- runLLVM ma s
  return (a, mod { moduleDefinitions = moduleDefinitions mod <> defs })

