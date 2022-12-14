module Main (main) where

import Tlang

import CLI.Parser (CommandLineOption (..), command)

import Control.Monad.Except
import System.Console.Haskeline
import qualified LLVM.AST as AST
import System.Environment
import LLVM.Context
import LLVM.Module
import LLVM.Internal.Target

process :: AST.Module -> (NameTable, TopSubstitution, Int) -> String -> IO (Maybe ((NameTable, TopSubstitution, Int), AST.Module))
process m (terms, env, c) line = do
    res'either <- runExceptT $ evalResolv env c line
    case res'either of
      Left err -> print err >> return Nothing
      Right ((tenv, counter), defs) -> do
        next'maybe <- genModule m terms defs
        return $ next'maybe >>= \(tb, next) -> return ((tb, tenv, counter), next)

repl :: IO ()
repl = runInputT defaultSettings (loop (createModule "stdin" "input") ([], moduleEnvironment, 0))
  where
      loop m (tb, tenv, c) = do
            minput <- getInputLine "repl> "
            case minput of
              Nothing -> outputStrLn "Goodbye."
              Just input -> do
                res'maybe <- liftIO $ process m (tb, tenv, c) input
                case res'maybe of
                  Nothing -> loop m (tb, tenv, c)
                  Just (group, nm) -> do
                    loop nm group

helpMsg :: [String]
helpMsg = [ "tool <command> {args}"
          , "commands"
          , "  help,--help,-h"
          , "  repl"
          , "  compile <input path> {output}"
          ]

printHelp :: IO ()
printHelp = mapM_ putStrLn helpMsg

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printHelp
    _ -> case command args of
     Left err -> do putStrLn "Got trouble? The input arguments seem wrong. Errors below: "
                    putStrLn $ show err
                    putStrLn "--------"
                    printHelp
     Right cmd -> do
       putStrLn $ show cmd
       case cmd of
          REPL -> repl
          HELP _ -> printHelp
          COMPILE ifile Nothing -> readFile ifile >>= process (createModule ifile ifile) ([], moduleEnvironment, 0) >> return ()
          COMPILE ifile (Just ofile) -> readFile ifile >>= process (createModule ifile ifile) ([], moduleEnvironment, 0) >>= \mod'maybe -> do
            case mod'maybe of
              Nothing -> return ()
              Just (_, transmod) -> withContext \context -> do
                withModuleFromAST context transmod \llvmMod -> do
                  initializeNativeTarget
                  withHostTargetMachineDefault \targetMachine -> do
                    writeObjectToFile targetMachine (File $ ofile ++ ".o") llvmMod

