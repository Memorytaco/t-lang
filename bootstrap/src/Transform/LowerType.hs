{-# LANGUAGE RankNTypes #-}
{- | This module is used to transform things into low level type (runtime type)

    TODO: This module should have similar structure to `Transform.TypeGraph` module
-}
module Transform.LowerType
  ( lowerTuple
  , lowerRecord

  , lowerTypeRep
  , lowerTypeRepPartial
  )
where

-- import Data.Kind (Type, Constraint)

import qualified Language.Core as AST
import Language.Core.Extension

import Tlang.Rep
import Tlang.Generic
-- import Tlang.Constraint (Prefix (..))

-- import Capability.Reader (HasReader)
import Control.Monad (forM)
import Data.Functor ((<&>))

-----------------------------------------------------------------------------------------
-- | lowering partial type, reduce some highlevel constructors into primitive constructor
--
-- if we choose to lower record type
--
-- @
-- e.g. lower (a, i8, c) = struct {a, i8#, c}
--      lower { name: str, age: int } = struct { #[ptr (bit 8)], int32 }
-- @
--
-- In this case, we have advantage to have same structure, using which we can
-- transfrom it into graphic type and do type unification.
--
-- lets say we have a type:
--
-- @
--    A := forall a b. {name: str, age: int, identity: a, property: b}
--    B := forall a. {name: str, age: int, identity: a, property: likelyint a}
-- @
--
-- and we have toplevel definition:
--
-- @
--    type likelyint a = int
-- @
--
-- we will always have this:
--
-- @
--    lower (forall a. likelyint a) = int32 // if parameter "a" is locally bound
-- @
-- 
-- and we choose not to lowering record type :
--
-- @
--    lower A = forall a b. { name: #[ptr (bit 8)], age, int32, identity: a, property: b}
--    lower B = forall a. { name: #[ptr (bit 8)], age: int32, identity: a, property: int32 }
-- @
--
-- unify A and B we got:
--
-- @
--    unify (lower A) (lower B) = { name: #[ptr (bit 8)], age: int32, identity: int32, property: int32 }
-- @
-- 
-- and we can finally lower the type:
--
-- @
--    lower (unify (lower A) (lower B)) = struct {#[ptr (bit 8)], int32, int32, int32}
-- @
--
-- but remember signature for @lower (unify (lower A) (lower B))@ is still @Type bind rep name a@
-- and we need a final pass to lower it into @Rep Name@ which is something trivial. This kind of
-- @Rep Name@ thing is what we get finally to do codegen. and we can choose to transform
-- it into LLVM IR type or C type.
-----------------------------------------------------------------------------------------

-- | partially lowering type
lowerTypeRepPartial
  :: (f :<: rep, Traversable bind, Traversable rep, Monad m) => (forall x. f (m (AST.Type bind rep name x)) -> m (AST.Type bind rep name x))
  -> AST.Type bind rep name a -> m (AST.Type bind rep name a)
lowerTypeRepPartial handle = cata go
  where
    go (AST.TypPhtF) = return AST.TypPht
    go (AST.TypVarF a) = return $ AST.TypVar a
    go (AST.TypConF m ms) = AST.TypCon <$> m <*> sequence ms
    go (AST.TypBndF fbind f) = do
      bodym <- lowerTypeRepPartial handle f
      body <- sequence $ bodym <&> sequence
      bind <- sequence fbind
      return $ AST.TypBnd bind body
    go (AST.TypeF rma) = case prj rma of
                           Just v -> handle v
                           Nothing -> AST.Type . inj <$> sequence rma

lowerTypeRep
  :: (f :~: (h :+: g), Rep :<: g, Traversable g, Functor f, Traversable bind, Monad m)
  => (forall x. h (m (AST.Type bind g name x)) -> m (AST.Type bind g name x))
  -> AST.Type bind f name a -> m (AST.Type bind g name a)
lowerTypeRep handle = cata go
  where
    go (AST.TypPhtF) = return AST.TypPht
    go (AST.TypVarF a) = return $ AST.TypVar a
    go (AST.TypConF m ms) = AST.TypCon <$> m <*> sequence ms
    go (AST.TypBndF fbind f) = do
      bodym <- lowerTypeRep handle f
      body <- sequence $ bodym <&> sequence
      bind <- sequence fbind
      return $ AST.TypBnd bind body
    go (AST.TypeF rma) = split handle (fmap (AST.Type). sequence) rma

lowerTuple :: (f :~: (Tuple :+: g), Rep :<: g, Traversable g, Functor f, Traversable bind, Monad m)
           => AST.Type bind f name a -> m (AST.Type bind g name a)
lowerTuple = lowerTypeRep \(Tuple ms) -> forM ms (Embed . DataRep <$>) <&> AST.Type . inj . Rep . RepLift . struct False

lowerRecord
  :: (f :~: (Record AST.Label :+: g), Rep :<: g, Traversable g, Functor f, Traversable bind, Monad m)
  => AST.Type bind f name a -> m (AST.Type bind g name a)
lowerRecord = lowerTypeRep \(Record (ms :: [(AST.Label, m (AST.Type bind g name a))])) ->
  forM (fmap sequence ms) (Embed . DataRep . snd <$>) <&> AST.Type . inj . Rep . RepLift . struct False
