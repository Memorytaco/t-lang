module Main (main) where

import CLI.Parser (getcommand, Command (..))

import LLVM.Context
import LLVM.Module
import LLVM.Internal.Target
import EvalLoop.Loop

-- process :: AST.Module -> (NameTable, TopSubstitution, Int) -> String -> IO (Maybe ((NameTable, TopSubstitution, Int), AST.Module))
process m (terms, env, c) line = do undefined
    -- res'either <- runExceptT $ evalResolv env c line
    -- case res'either of
    --   Left err -> print err >> return Nothing
    --   Right ((tenv, counter), defs) -> do
    --     next'maybe <- genModule m terms defs
    --     return $ next'maybe >>= \(tb, next) -> return ((tb, tenv, counter), next)

main :: IO ()
main = do
  cmd <- getcommand
  case cmd of
    C'help (Just topic) -> do
      putStrLn $ "Sorry, no info for " <> topic
    C'help Nothing -> do
      putStrLn "Please use -h|--help for more information of available commands"
    C'repl -> shell
    C'compile ifile Nothing -> readFile ifile >>= process () ([], (), 0) >> return ()
    C'compile ifile (Just ofile) ->
      readFile ifile >>= process () ([], (), 0) >>= \mod'maybe -> do
      case mod'maybe of
        Nothing -> return ()
        Just (_, transmod) -> withContext \context -> do
          withModuleFromAST context transmod \llvmMod -> do
            initializeNativeTarget
            withHostTargetMachineDefault \targetMachine -> do
              writeObjectToFile targetMachine (File $ ofile ++ ".o") llvmMod
    C'none -> return ()
