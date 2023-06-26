module Driver.Inference
  ( defaultEnv
  )
where

-- import Tlang.Inference.Kind (NormalKind)
import Tlang.AST (Symbol (..), type (@:) (..), Kind (..), Symbol)

-- defaultEnv :: [NormalKind :@ Symbol]
defaultEnv = undefined
--   [ Op "->" :@ (KindType ::> KindType ::> KindType)
--   , Symbol "maybe" :@ (KindType ::> KindType)
--   , Symbol "i8" :@ KindType
--   , Symbol "str" :@ KindType
--   , Symbol "g1" :@ (KindType ::> KindType ::> KindType ::> KindType)
--   ]
