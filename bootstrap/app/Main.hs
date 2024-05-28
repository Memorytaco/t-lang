module Main (main) where

import CLI.Parser (getcommand, Command (..))

import EvalLoop.Loop

main :: IO ()
main = do
  cmd <- getcommand
  case cmd of
    C'help (Just topic) -> do
      putStrLn $ "Sorry, no info for " <> topic
    C'help Nothing -> do
      putStrLn "Please use -h|--help for more information of available commands"
    C'repl -> evaDriver (Just ".yohisto")
    C'compile _ Nothing -> error "not implemented yet"
    C'compile _ (Just _) -> error "not implemented yet"
    C'none -> return ()
