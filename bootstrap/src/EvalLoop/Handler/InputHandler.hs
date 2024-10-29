module EvalLoop.Handler.InputHandler
  ( Haskeline
  , getUserInput
  , putStrLine
  , putStrOut
  , handleHaskeline
  )
where


import Effectful
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (interpret)

import System.Console.Haskeline (Settings, getInputLine, runInputT)
import Effectful.Dispatch.Static (unsafeEff_)

data Haskeline :: Effect where
  GetUserInput :: String -> Haskeline m (Maybe String)
  PutStrLine   :: String -> Haskeline m ()
  PutStrOut    :: String -> Haskeline m ()

makeEffect ''Haskeline

handleHaskeline :: IOE :> es => Settings (Eff es) -> Eff (Haskeline: es) a -> Eff es a
handleHaskeline settings = interpret \_ -> \case
  GetUserInput prompt -> runInputT settings do getInputLine prompt
  PutStrLine str -> unsafeEff_ $ putStrLn str
  PutStrOut str -> unsafeEff_ $ putStr str
