{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module JIT.LLVM
  (

  -- ** JIT Context
    LLVMJITContext
  , createLLVMJITContext
  , createLLVMJITContextDefault
  , disposeLLVMJITContext

  -- ** JIT Lib
  , JITClass
  , getJITName
  , newJITClass

  -- *** Operators for JIT Lib
  , addLLVMModule
  , addLLVMModuleFromAST

  , addDylibFile
  , addObjectFile
  , addLinkOrder
  , addLinkOrders
  , removeLinkOrder
  , removeLinkOrders

  -- ** JIT Symbol
  , JITError (..)
  , LLVM.JITSymbol
  , lookupSymbol
  , callJITSymbol
  , castPtr
  , fetchFunPtr

  , module FFI
  )
where

import Foreign.LibFFI as FFI
import qualified Foreign.Ptr as FFI

import qualified LLVM.Context as LLVM
import qualified LLVM.OrcJIT as LLVM
import qualified LLVM.Module as LLVM
import qualified LLVM.Target as LLVM
import qualified LLVM.Relocation as ModelR
import qualified LLVM.CodeModel as ModelC
import qualified LLVM.CodeGenOpt as Opt
import qualified LLVM.AST as LAST

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS

import Control.Monad (forM_)
import Control.Monad.IO.Class

data LLVMJITContext
  = forall ir obj. (LLVM.IRLayer ir, LLVM.ObjectLayer obj)
  => LLVMJITContext
    { llvmJITsession :: LLVM.ExecutionSession
    , llvmJITobjLayer :: obj
    , llvmJITirLayer :: ir
    , llvmJITloadedDylib :: [(String, LLVM.JITDylib)]
    , llvmJITmodules :: [LLVM.Module]
    }

-- | a wrapper for managing symbol tables
data JITClass
  = JITClass
    { getJITName :: String
    , getJITLib :: LLVM.JITDylib
    }

-- | create context from a target machine
createLLVMJITContext :: MonadIO m => LLVM.TargetMachine -> m LLVMJITContext
createLLVMJITContext machine = liftIO $ do
  session <- LLVM.createExecutionSession
  objLayer <- LLVM.createRTDyldObjectLinkingLayer session
  irLayer <- LLVM.createIRCompileLayer session objLayer machine
  return $ LLVMJITContext session objLayer irLayer [] []

-- | create context from default configuration
createLLVMJITContextDefault :: MonadIO m => m LLVMJITContext
createLLVMJITContextDefault =
  liftIO $ LLVM.withHostTargetMachine ModelR.PIC ModelC.JITDefault Opt.Default createLLVMJITContext

disposeLLVMJITContext :: MonadIO m => LLVMJITContext -> m ()
disposeLLVMJITContext (LLVMJITContext { llvmJITsession }) = liftIO $ LLVM.disposeExecutionSession llvmJITsession

-- | add a dynamic library file on system to current context
addDylibFile :: MonadIO m => (String, FilePath) -> LLVMJITContext -> m (LLVMJITContext, JITClass)
addDylibFile (name, path) ctx@(LLVMJITContext { llvmJITirLayer }) = liftIO $ do
  (ctx', jitClass) <- case lookup name (llvmJITloadedDylib ctx) of
    Just dylib -> return (ctx, JITClass name dylib)
    Nothing -> newJITClass name ctx
  LLVM.addDynamicLibrarySearchGenerator llvmJITirLayer (getJITLib jitClass) path
  return (ctx', jitClass)

addObjectFile :: MonadIO m => (String, FilePath) -> LLVMJITContext -> m (LLVMJITContext, JITClass)
addObjectFile (name, path) ctx@(LLVMJITContext { llvmJITobjLayer } ) = liftIO $ do
  (ctx', jitClass) <- case lookup name (llvmJITloadedDylib ctx) of
    Just dylib -> return (ctx, JITClass name dylib)
    Nothing -> newJITClass name ctx
  LLVM.addObjectFile llvmJITobjLayer (getJITLib jitClass) path
  return (ctx', jitClass)

-- create new jitclass to be used
newJITClass :: MonadIO m => String -> LLVMJITContext -> m (LLVMJITContext, JITClass)
newJITClass name ctx@(LLVMJITContext {..}) = liftIO $ do
  lib <- LLVM.createJITDylib llvmJITsession (BS.toShort $ B8.pack name)
  return (ctx { llvmJITloadedDylib = (name, lib):llvmJITloadedDylib }, JITClass name lib)

addLinkOrder :: MonadIO m => JITClass -> JITClass -> m ()
addLinkOrder (JITClass _ dylib) (JITClass _ order) = liftIO $ LLVM.addLinkAgainstOrder dylib order

addLinkOrders :: MonadIO m => JITClass -> [JITClass] -> m ()
addLinkOrders dylib libs = forM_ libs $ addLinkOrder dylib

removeLinkOrder :: MonadIO m => JITClass -> JITClass -> m ()
removeLinkOrder (JITClass _ dylib) (JITClass _ order) = liftIO $ LLVM.removeLinkAgainstOrder dylib order

removeLinkOrders :: MonadIO m => JITClass -> [JITClass] -> m ()
removeLinkOrders dylib libs = forM_ libs $ removeLinkOrder dylib

-- | the origin llvm module is intact, this adds a module to jit context
addLLVMModule :: MonadIO m => String -> LLVM.Module -> LLVMJITContext -> m (LLVMJITContext, JITClass)
addLLVMModule name m ctx@(LLVMJITContext { llvmJITirLayer }) = liftIO $ do
  (ctx', jitClass) <- newJITClass name ctx
  LLVM.withClonedThreadSafeModule m \tsm ->
    LLVM.addModule tsm (getJITLib jitClass) llvmJITirLayer
  return (ctx' { llvmJITmodules = m: llvmJITmodules ctx' }, jitClass)

-- | this add an ast module to jit context, it may throw exception. See `LLVM.Module`
addLLVMModuleFromAST :: MonadIO m => String -> LAST.Module -> LLVMJITContext -> m (LLVMJITContext, JITClass)
addLLVMModuleFromAST name am ctx = liftIO $
  LLVM.withContext \lctx ->
    LLVM.withModuleFromAST lctx am \m ->
      addLLVMModule name m ctx

data JITError
  = JITNotFound
  | JITNotCallable
  | JITError String
  deriving (Show, Eq, Ord)

-- | lowlevel symbol lookup
lookupSymbol :: MonadIO m => String -> JITClass -> LLVMJITContext -> m (Either JITError LLVM.JITSymbol)
lookupSymbol name (getJITLib -> lib) (LLVMJITContext { llvmJITsession, llvmJITirLayer }) = liftIO $
  LLVM.lookupSymbol llvmJITsession llvmJITirLayer lib (BS.toShort $ B8.pack name)
  >>= \case
  Left (LLVM.JITSymbolError bs) -> return . Left . JITError . B8.unpack $ BS.fromShort bs
  Right r -> return (Right r)

fetchFunPtr :: LLVM.JITSymbol -> FFI.FunPtr a
fetchFunPtr = FFI.castPtrToFunPtr . castPtr

castPtr :: LLVM.JITSymbol -> FFI.Ptr a
castPtr = FFI.wordPtrToPtr . LLVM.jitSymbolAddress

callJITSymbol :: MonadIO m => LLVM.JITSymbol -> FFI.RetType b -> [FFI.Arg] -> m b
callJITSymbol sym = fmap liftIO . FFI.callFFI (fetchFunPtr sym)
