module CLI.Parser
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

-- | help subcommand
chelp = C'help
  <$> (optional $ strArgument (metavar "TOPIC" <> help "show info on the topic"))

crepl = pure C'repl

ccompile = C'compile
  <$> strArgument (metavar "src" <> help "source file")
  <*> (optional $ strArgument (metavar "dst" <> help "object file"))

commands =
  hsubparser $
      command "help" (info chelp $ progDesc "help with designated topic")
  <>  command "repl" (info crepl $ progDesc "start read-eval-loop")
  <>  command "compile" (info ccompile $ progDesc "compile source file")

getcommand = execParser (info ((commands <|> pure C'none) <**> helper) $ progDesc "compiler and interpreter for the language")
