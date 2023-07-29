{-| * User interface module
-}

module EvalLoop.Loop
  ( repl'loop
  , shell
  )
where

import Data.Text (Text, pack)
import System.Console.Haskeline
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadMask)
import Text.Megaparsec
import Control.Monad.State (MonadState (..), StateT (..), modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor (($>))

import Driver.Parser
import qualified EvalLoop.Read as Helper
import qualified EvalLoop.Config as Helper
import Language.Core (builtinStore, query)
import Language.Core.Extension.Decl (Item (..), UserOperator (..))

import Control.Lens

data LoopControl state
  = LoopControl
    { isClosed :: Bool
    , rPrompt :: String
    , rState :: state
    } deriving (Show, Eq)

-- makeLenses ''LoopControl

shell :: IO ()
shell =  runInputT defaultSettings loop $> ()
  where loop = runStateT repl'loop (LoopControl False "$ > " (conf, stat))
        conf = Helper.ShellConfig (Helper.SearchEnv [] [])
        stat = Helper.ShellState 0 builtinStore []

repl'loop :: (MonadIO m, MonadMask m) => StateT (LoopControl (Helper.ShellConfig, Helper.ShellState PredefDeclExtVal)) (InputT m) ()
repl'loop = do
  status <- get
  line'maybe <- lift . getInputLine $ rPrompt status
  case line'maybe of
    Nothing ->
      if isClosed status
      then lift $ outputStrLn "done."
      else do
        lift $ outputStrLn "Prese ^D again to exit"
        modify (\s -> s { isClosed = True })
        repl'loop
    Just txt -> do
      (pRes'either, stat, _) <- uncurry Helper.runToplevel (rState status) (pack txt)
      modify (\s -> s { rState = const stat <$> rState s})
      case pRes'either of
        Left bundle'err -> lift . outputStrLn $ errorBundlePretty bundle'err
        Right res -> do
          case res of
            Helper.ReadCommand cmd ->
              case cmd of
                Helper.RDefineGlobal decl -> do
                  case query @(Item (UserOperator Text)) (const True) decl of
                    Just (Item (UserOperator op) _) ->
                      modify (\s -> s { rState = (\ss -> ss { Helper.operators = (op:) $ Helper.operators ss}) <$> rState s})
                    Nothing -> return ()
                  lift . outputStrLn $ show decl
            Helper.ReadExpr exp -> lift . outputStrLn $ show exp
            Helper.ReadNothing -> return ()
      modify (\s -> s { isClosed = False })
      repl'loop

