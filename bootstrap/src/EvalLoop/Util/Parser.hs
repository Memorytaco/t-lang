module EvalLoop.Util.Parser
  ( parseSurfaceExpr
  )
where

import Language.Core (OperatorStore, ExprSurface, TypSurface)
import Language.Parser

import Driver.Parser

import Text.Megaparsec
import Data.Text (Text)
import Data.Void (Void)

parseSurfaceExpr
  :: Monad m => String -> OperatorStore -> Text
  -> m (Either (ParseErrorBundle Text Void) (ExprSurface TypSurface), OperatorStore)
parseSurfaceExpr prompt ops = driveParser ops (runDSL @(PredefExprLang _) @(ExprSurface TypSurface) eof) prompt
