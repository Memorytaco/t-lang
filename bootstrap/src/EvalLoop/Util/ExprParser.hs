module EvalLoop.Util.ExprParser
  ( parseSurfaceExpr
  )
where

import Language.Core (OperatorStore)
import Language.Parser

import Driver.Parser

import Text.Megaparsec
import Data.Text (Text)
import Data.Void (Void)

parseSurfaceExpr
  :: Monad m => String -> OperatorStore -> Text
  -> m (Either (ParseErrorBundle Text Void) PredefExprVal, OperatorStore)
parseSurfaceExpr prompt ops = driveParser ops (runDSL @(PredefExprLang _) @PredefExprVal eof) prompt
