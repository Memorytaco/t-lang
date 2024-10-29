module Commandline.Parser
  ( Command (..)
  , commands
  , getcommand
  )
where

import Options.Applicative

data Command
  = C'help (Maybe String)
  | C'compile String (Maybe String)
  | C'repl
  | C'none
  deriving (Show, Eq, Ord)

chelp, crepl, ccompile, commands :: Parser Command

-- | help subcommand
chelp = C'help
  <$> optional (strArgument (metavar "TOPIC" <> help "show info on the topic"))

-- | repl subcommand
crepl = pure C'repl

-- | compile subcommand
ccompile = C'compile
  <$> strArgument (metavar "src" <> help "source file")
  <*> optional (strArgument (metavar "dst" <> help "object file"))

-- | toplevel parser
commands =
  hsubparser $
      command "help" (info chelp $ progDesc "help with designated topic")
  <>  command "repl" (info crepl $ progDesc "start read-eval-loop")
  <>  command "compile" (info ccompile $ progDesc "compile source file")

-- | parse command line options
getcommand :: IO Command
getcommand = execParser
  (info ((commands <|> pure C'none) <**> helper)
    $ progDesc "compiler and interpreter for the language")

