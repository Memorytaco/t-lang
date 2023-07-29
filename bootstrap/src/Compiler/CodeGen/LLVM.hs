module Compiler.CodeGen.LLVM
  (
    globalDefine
  , globalFunction
  , externFunction
  , globalType
  )
where

import qualified LLVM.IRBuilder as Builder
import qualified LLVM.AST as IR
import qualified LLVM.AST.Global as IR
import qualified LLVM.AST.Constant as IR

import Control.Monad.State (modify)
import LLVM.IRBuilder.Internal.SnocList (getSnocList, snoc)

import Control.Monad (forM)

import Data.ByteString.Short (ShortByteString)

import Compiler.CodeGen.LLVM.IR
import Data.Functor ((<&>))

globalDefine :: Builder.MonadModuleBuilder m => IR.Definition -> m ()
globalDefine def = Builder.liftModuleState $ modify \s ->
  s { Builder.builderDefs = Builder.builderDefs s `snoc` def }

-- | Define a (non-variadic) global function with c calling convention
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

externFunction :: Builder.MonadModuleBuilder m => IR.Name -> [IR.Type] -> IR.Type -> m IR.Operand
externFunction fname paras retType = do
  globalDefine . IR.GlobalDefinition $ IR.functionDefaults
    { IR.name = fname
    , IR.parameters = (zip paras [0..] <&> \(typ, name) -> IR.Parameter typ (IR.UnName name) [], False)
    , IR.returnType = retType
    }
  return $ IR.ConstantOperand $ IR.GlobalReference fname

globalType :: Builder.MonadModuleBuilder m => IR.Name -> Maybe IR.Type -> m IR.Type
globalType tname typ'maybe = do
  globalDefine $ IR.TypeDefinition tname typ'maybe
  return (IR.NamedTypeReference tname)
