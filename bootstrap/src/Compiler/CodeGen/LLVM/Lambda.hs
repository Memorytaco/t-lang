{- | Helper functions used for generating lambda expression.
--
-}
module Compiler.CodeGen.LLVM.Lambda
  (
    genLambda
  , obtainLambdaName
  )
where


import Capability.State (HasState, modify, gets, get)
import LLVM.AST (Operand)
import Language.Core (Expr)
import qualified Data.Map as Map
import qualified LLVM.AST as LLVM
import Data.Maybe (fromMaybe)

type CanGenLambda m = (HasState "Lambda.Scope" String m, HasState "Lambda.Prefix" (Map.Map String Integer) m)

-- | generate a module level unique function identifier
obtainLambdaName :: CanGenLambda m => String -> m LLVM.Name
obtainLambdaName name = do
  counter <- gets @"Lambda.Prefix" (fromMaybe 0 . Map.lookup name)
  modify @"Lambda.Prefix" (Map.insert name (counter+1))
  return (LLVM.mkName (name <> ".lambda" <> show counter))


genLambda :: CanGenLambda m => Expr f a -> m Operand
genLambda _e = do
  _fname <- get @"Lambda.Scope" >>= obtainLambdaName
  undefined