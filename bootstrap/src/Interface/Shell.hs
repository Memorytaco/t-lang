{-| * User interface module
-}

module Interface.Shell
  ( repl'loop
  , shell
  )
where

import Data.Text (pack)
import System.Console.Haskeline
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadMask)
import Text.Megaparsec
import Data.Bifunctor (second)
import Control.Monad.State (MonadState (..), StateT (..), modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor (($>))

import Driver.Parser
import qualified Interface.Parser as Helper
import qualified Interface.Config as Helper
import Tlang.AST (typOperator, Decl (..))
import Tlang.Extension.Decl (UserItem (..))
import Tlang.AST.Class.Decl (query)

data ReplStatus state
  = ReplStatus
    { isClosed :: Bool
    , rPrompt :: String
    , rState :: state
    } deriving (Show, Eq)

shell :: IO ()
shell =  runInputT defaultSettings loop $> ()
  where loop = runStateT repl'loop (ReplStatus False "$ > " (conf, stat))
        conf = Helper.ShellConfig (Helper.SearchEnv [] [])
        stat = Helper.ShellState 0 (typOperator, [])

repl'loop :: (MonadIO m, MonadMask m) => StateT (ReplStatus (Helper.ShellConfig, Helper.ShellState)) (InputT m) ()
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
      case pRes'either of
        Left bundle'err -> lift . outputStrLn $ errorBundlePretty bundle'err
        Right res -> do
          case res of
            Helper.LangDef decl txt -> do
              case query @UserItem (const True) decl of
                Just (UserItem _ ops _) -> modify (\s -> s { rState = second (Helper.addTermOperators ops) $ rState s})
                Nothing -> return ()
              lift . outputStrLn $ show decl
            Helper.LangExpr exp txt -> lift . outputStrLn $ show exp
            Helper.LangNone -> return ()
      modify (\s -> s { isClosed = False })
      repl'loop

