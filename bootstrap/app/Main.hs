module Main (main) where

import Tlang

import CLI.Parser (getcommand, Command (..))

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
              Nothing -> outputStrLn "done."
              Just input -> do
                res'maybe <- liftIO $ process m (tb, tenv, c) input
                case res'maybe of
                  Nothing -> loop m (tb, tenv, c)
                  Just (group, nm) -> do
                    loop nm group

main :: IO ()
main = do
  cmd <- getcommand
  case cmd of
    C'help (Just topic) -> do
      putStrLn "Sorry, no topic for " <> topic
    C'help Nothing -> do
      putStrLn "Please use -h|--help for more information of available commands"
    C'repl -> repl
    C'compile ifile Nothing -> readFile ifile >>= process (createModule ifile ifile) ([], moduleEnvironment, 0) >> return ()
    C'compile ifile (Just ofile) ->
      readFile ifile >>= process (createModule ifile ifile) ([], moduleEnvironment, 0) >>= \mod'maybe -> do
      case mod'maybe of
        Nothing -> return ()
        Just (_, transmod) -> withContext \context -> do
          withModuleFromAST context transmod \llvmMod -> do
            initializeNativeTarget
            withHostTargetMachineDefault \targetMachine -> do
              writeObjectToFile targetMachine (File $ ofile ++ ".o") llvmMod
    C'none -> return ()
