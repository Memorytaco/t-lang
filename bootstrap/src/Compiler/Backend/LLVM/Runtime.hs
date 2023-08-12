{- | a helper module which provides many common codegen pattern with
-- carefully designed environment.
--
-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Compiler.Backend.LLVM.Runtime
  (
    wrapMain
  , globalString
  , unitConstant


  -- ** Constants builder

  -- *** Simple and Complex Constants
  , Bits (..)
  , buildCString
  , buildCArray
  , buildCInt
  , buildCFloat
  , buildCDouble
  , buildCStruct
  , buildCOperand
  , constantOperand
  )
where

import Compiler.Backend.LLVM.Definition
    ( globalFunction, MonadLLVMBuilder, globalDefine, MonadModuleBuilder )
import Compiler.Backend.LLVM.IR (ensureNamedBlock, ensureBlockEndWith)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import qualified LLVM.AST.Constant as LLVM
import Data.String (IsString)
import Data.Char (ord)
import qualified LLVM.AST.Global as LLVM
import qualified LLVM.AST.Linkage as LLVM
import qualified LLVM.AST.Global as LLVM.Global
import qualified LLVM.AST.Float as LLVM
import Control.Lens ((<&>))


-- | make a main function as execution environment for generated code.
wrapMain :: (MonadLLVMBuilder m, IsString name) => ([(name, (LLVM.Type, LLVM.Operand))] -> m LLVM.Operand) -> m LLVM.Operand
wrapMain make = globalFunction "main" [(LLVM.i32, Just "argc"), (LLVM.ptr, Just "argv")] LLVM.i32 \case
  [argc, argv] -> do
    -- create start block
    ensureNamedBlock "main.start"
    r <- make [("argc", (LLVM.i32, argc)), ("argv", (LLVM.ptr, argv))]
    -- create ending block
    LLVM.typeOf r >>= \case
      Right (LLVM.IntegerType 32) -> ensureBlockEndWith (LLVM.Do $ LLVM.Ret (Just r) [])
      _ -> ensureBlockEndWith . LLVM.Do $ LLVM.Ret (Just $ LLVM.ConstantOperand $ LLVM.Int 32 0) []
  _ -> error "impossible when building wrapper for main function"

globalString :: MonadModuleBuilder m => String -> LLVM.Name -> m LLVM.Operand
globalString content name = do
  (typ, str) <- return $ buildCString content
  globalDefine . LLVM.GlobalDefinition $ LLVM.globalVariableDefaults
    { LLVM.name = name
    , LLVM.linkage = LLVM.External
    , LLVM.isConstant = True
    , LLVM.initializer = Just str
    , LLVM.Global.type' = typ
    }
  return (LLVM.ConstantOperand $ LLVM.GlobalReference name)

-- | constant used for representing `()` 
unitConstant :: (LLVM.Type, LLVM.Constant)
unitConstant = buildCInt 1 0

-------------------------
-- ** Constants builder
-------------------------

-- | string is always 0 ended.
buildCString :: String -> (LLVM.Type, LLVM.Constant)
buildCString content =
  let eles = content <&> snd . buildCInt 8 . toInteger . ord
      (typ, end) = buildCInt 8 0
   in buildCArray typ (eles <> [end])

buildCArray :: LLVM.Type -> [LLVM.Constant] -> (LLVM.Type, LLVM.Constant)
buildCArray typ vals = (LLVM.ArrayType (fromIntegral $ length vals) typ, LLVM.Array typ vals)
{-# INLINE buildCArray #-}

newtype Bits = Bits { getBits :: Integer } deriving (Show, Eq, Ord, Enum, Num, Real) via Integer

buildCInt :: Bits -> Integer -> (LLVM.Type, LLVM.Constant)
buildCInt (Bits (fromInteger -> bits)) val = (LLVM.IntegerType bits, LLVM.Int bits val)
{-# INLINE buildCInt #-}

buildCFloat :: Float -> (LLVM.Type, LLVM.Constant)
buildCFloat val = (LLVM.FloatingPointType LLVM.FloatFP, LLVM.Float (LLVM.Single val))
{-# INLINE buildCFloat #-}

buildCDouble :: Double -> (LLVM.Type, LLVM.Constant)
buildCDouble val = (LLVM.FloatingPointType LLVM.DoubleFP, LLVM.Float (LLVM.Double val))
{-# INLINE buildCDouble #-}

buildCStruct :: Maybe LLVM.Name -> Bool -> [(LLVM.Type, LLVM.Constant)] -> (LLVM.Type, LLVM.Constant)
buildCStruct name'maybe isPacked fields =
  case name'maybe of
    Just name -> (LLVM.NamedTypeReference name, constant)
    Nothing -> (LLVM.StructureType isPacked $ fst <$> fields, constant)
  where
    constant = LLVM.Struct name'maybe isPacked $ snd <$> fields

buildCOperand :: (LLVM.Type, LLVM.Constant) -> (LLVM.Type, LLVM.Operand)
buildCOperand = fmap LLVM.ConstantOperand
{-# INLINE buildCOperand #-}

constantOperand :: LLVM.Constant -> LLVM.Operand
constantOperand = LLVM.ConstantOperand
{-# INLINE constantOperand #-}

-------------------------
-- ** Instruction builder
-------------------------
