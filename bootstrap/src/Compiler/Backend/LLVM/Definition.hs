{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Compiler.Backend.LLVM.Definition
  (
    globalDefine
  , globalFunction
  , externFunction
  , globalNamedType

  , MonadLLVMBuilder
  , Builder.MonadModuleBuilder
  , Builder.MonadIRBuilder
  )
where

import qualified LLVM.IRBuilder as Builder
import qualified LLVM.AST as IR
import qualified LLVM.AST.Global as IR
import qualified LLVM.AST.Constant as IR
import LLVM.IRBuilder.Internal.SnocList (snoc)

import Control.Monad (forM, forM_)
import Control.Monad.State (modify)

import Data.ByteString.Short (ShortByteString)
import Data.Functor ((<&>))
import qualified Data.Map as Map

import Compiler.Backend.LLVM.IR
    ( fresh, named, safeIRBuilderState, flushBasicBlocks )

-- | a shorthand for ModuleBuilder and IRBuilder
type MonadLLVMBuilder m = (Builder.MonadModuleBuilder m, Builder.MonadIRBuilder m)

-- | generate arbitrary global definition
globalDefine :: Builder.MonadModuleBuilder m => IR.Definition -> m ()
globalDefine def = Builder.liftModuleState $ modify \s ->
  s { Builder.builderDefs = Builder.builderDefs s `snoc` def }

-- | define a (non-variadic) global function with c calling convention
globalFunction
  :: (Builder.MonadModuleBuilder m, Builder.MonadIRBuilder m)
  => IR.Name -> [(IR.Type, Maybe ShortByteString)] -> IR.Type
  -> ([IR.Operand] -> m ())
  -> m IR.Operand
globalFunction fname paras retType genBody = do
  (paraNames, blocks) <- safeIRBuilderState Builder.emptyIRBuilder do
    fparas <- forM paras \(typ, name'maybe) -> (typ,) <$>
      case name'maybe of
        Nothing -> fresh
        Just name -> fresh `named` name
    genBody $ uncurry IR.LocalReference <$> fparas
    blocks <- flushBasicBlocks
    return (fparas, blocks)
  globalDefine . IR.GlobalDefinition $ IR.functionDefaults
    { IR.name = fname
    , IR.parameters = (paraNames >>= \(typ, name) -> pure $ IR.Parameter typ name [], False)
    , IR.returnType = retType
    , IR.basicBlocks = blocks
    }
  return $ IR.ConstantOperand $ IR.GlobalReference fname

-- | declare an external function
externFunction :: Builder.MonadModuleBuilder m => IR.Name -> [IR.Type] -> IR.Type -> m IR.Operand
externFunction fname paras retType = do
  globalDefine . IR.GlobalDefinition $ IR.functionDefaults
    { IR.name = fname
    , IR.parameters = (zip paras [0..] <&> \(typ, name) -> IR.Parameter typ (IR.UnName name) [], False)
    , IR.returnType = retType
    }
  return $ IR.ConstantOperand $ IR.GlobalReference fname

-- | generate named opaque type or named structure type
globalNamedType :: Builder.MonadModuleBuilder m => IR.Name -> Maybe IR.Type -> m IR.Type
globalNamedType tname typ'maybe = do
  globalDefine $ IR.TypeDefinition tname typ'maybe
  forM_ typ'maybe \typ -> Builder.liftModuleState $ modify \s ->
    -- this map seems to be used for `typeOf` reverse type lookup
    s { Builder.builderTypeDefs = Map.insert tname typ (Builder.builderTypeDefs s) }
  return (IR.NamedTypeReference tname)
