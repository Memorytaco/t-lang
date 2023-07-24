module JIT.JIT
  ( runJIT )
where

import LLVM.AST (Module, mkName)
import LLVM.Context (Context, withContext)
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module (withModuleFromAST)
import qualified Foreign.LibFFI as FFI

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c Nothing Nothing Nothing Nothing

-- Expect a main function in the module
runJIT :: String -> Module -> IO ()
runJIT name amod =
  withContext \context ->
  withModuleFromAST context amod \m ->
  jit context \engine ->
  EE.withModuleInEngine engine m \ee -> do
    f'maybe <- EE.getFunction ee (mkName name)
    case f'maybe of
      Nothing -> fail $ "Couldn't find " <> name
      Just f -> do
        res <- FFI.callFFI f FFI.retCInt []
        putStrLn $ "Result: " <> show res
