module Main (main) where

import Tlang

import CLI.Parser (CommandLineOption (..), command)

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import qualified LLVM.AST as AST
import System.Environment
import LLVM.Context
import LLVM.Module
import LLVM.Internal.Target

process :: AST.Module -> String -> IO (Maybe AST.Module)
process transmod line = do
    let res = parseToplevel line
    case res of
      Left err -> print err >> return Nothing
      Right ex -> case fst . execResolv . fmap sequence $ mapM resolve ex of
        Left err -> print err >> return Nothing
        Right exTyped -> do
          mapM_ print exTyped
          modUpdate <- genModule transmod exTyped
          return $ Just modUpdate

repl :: IO ()
repl = runInputT defaultSettings (loop (createModule "stdin"))
  where
      loop transmod = do
            minput <- getInputLine "ready> "
            case minput of
              Nothing -> outputStrLn "Goodbye."
              Just input -> do
                mod'maybe <- liftIO $ process transmod input
                loop (maybe transmod id mod'maybe)

helpMsg = [ "tool <command> {args}"
          , "commands"
          , "  help,--help,-h"
          , "  repl"
          , "  compile <input path> {output}"
          ]

printHelp = mapM_ putStrLn helpMsg

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printHelp
    _ -> case command args of
     Left err -> putStrLn $ show err
     Right cmd -> do
       putStrLn $ show cmd
       case cmd of
          REPL -> repl
          HELP _ -> printHelp
          COMPILE ifile Nothing -> readFile ifile >>= process (createModule ifile) >> return ()
          COMPILE ifile (Just ofile) -> readFile ifile >>= process (createModule ifile) >>= \mod'maybe -> do
            case mod'maybe of
              Nothing -> return ()
              Just transmod -> withContext \context -> do
                withModuleFromAST context transmod \llvmMod -> do
                  initializeNativeTarget
                  withHostTargetMachineDefault \targetMachine -> do
                    writeObjectToFile targetMachine (File $ ofile ++ ".o") llvmMod

