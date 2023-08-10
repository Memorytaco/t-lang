module Driver.Inference
  ( defaultEnv
  )
where

-- defaultEnv :: [NormalKind :@ Symbol]
defaultEnv :: a
defaultEnv = undefined
--   [ Op "->" :@ (KindType ::> KindType ::> KindType)
--   , Symbol "maybe" :@ (KindType ::> KindType)
--   , Symbol "i8" :@ KindType
--   , Symbol "str" :@ KindType
--   , Symbol "g1" :@ (KindType ::> KindType ::> KindType ::> KindType)
--   ]
