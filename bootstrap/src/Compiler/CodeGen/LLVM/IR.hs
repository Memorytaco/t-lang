{- | A wrapper to LLVM.IRBuilder for IR
  -}
module Compiler.CodeGen.LLVM.IR
  (
    safeIRBuilderState
  , ensureBlockEnd
  , ensureBlockEndWith
  , ensureNamedBlock
  , ensureBlock
  , startNewBlock
  , startNewBlockWith
  , startNewNamedBlock
  , startNewNamedBlockWith
  , flushBasicBlocks
  , flushBasicBlocksWith

  -- ** re-export
  , Builder.fresh
  , Builder.freshName
  , Builder.named
  )
where


import qualified LLVM.IRBuilder as Builder
import LLVM.IRBuilder.Internal.SnocList (getSnocList, snoc)
import qualified LLVM.AST as IR

import Data.Monoid (First (..))
import Data.Functor (($>))
import Control.Monad.State (get, gets, put, modify)

-- | Replace current IRBuilderState with a new one and return old one
popIRBuilderState :: Builder.MonadIRBuilder m => Builder.IRBuilderState -> m Builder.IRBuilderState
popIRBuilderState initState = Builder.liftIRState do
  get >>= \stat -> put initState $> stat
{-# INLINE popIRBuilderState #-}

-- | a shorthand for `liftIRState . put`
pushIRBuilderState :: Builder.MonadIRBuilder m => Builder.IRBuilderState -> m ()
pushIRBuilderState = Builder.liftIRState . put
{-# INLINE pushIRBuilderState #-}

-- | use another IRBuilderState to serve incoming computation, after computation terminated,
-- restore origin IRBuilderState. alas, safe computation with current state intact.
--
-- This is useful when you need to jump into another environment to emit IR like another
-- function block.
safeIRBuilderState :: Builder.MonadIRBuilder m => Builder.IRBuilderState -> m a -> m a
safeIRBuilderState initState m = do
  stat <- popIRBuilderState initState
  ensureBlock *> m <* pushIRBuilderState stat

-- | end current block if there is one. After this, there is no active block.
--
-- If there is no terminator in current block then add a "ret operand" or "ret" terminator.
ensureBlockEnd :: Builder.MonadIRBuilder m => Maybe IR.Operand -> m ()
ensureBlockEnd val = ensureBlockEndWith (IR.Do (IR.Ret val []))
{-# INLINE ensureBlockEnd #-}

-- | end current block if there is one. After this, there is no active block.
--
-- If there is no terminator in current block then append provided terminator.
ensureBlockEndWith :: Builder.MonadIRBuilder m => IR.Named IR.Terminator -> m ()
ensureBlockEndWith end = do
  pb'maybe <- Builder.liftIRState $ gets Builder.builderBlock
  case pb'maybe of
    Nothing -> return ()
    Just pb -> Builder.liftIRState . modify $ \s ->
      s { Builder.builderBlocks = Builder.builderBlocks s `snoc` bb
        }
      where
        instrs = getSnocList $ Builder.partialBlockInstrs pb
        name = Builder.partialBlockName pb
        bb = case getFirst (Builder.partialBlockTerm pb) of
               Nothing -> IR.BasicBlock name instrs end
               Just term -> IR.BasicBlock name instrs term
  Builder.liftIRState $ modify $ \s ->
    s { Builder.builderBlock = Nothing
      }

-- | make sure there is always an active block there.
--
-- A new block is created with assigned block name if there is no active block.
ensureNamedBlock :: Builder.MonadIRBuilder m => IR.Name -> m ()
ensureNamedBlock name = do
  pb'maybe <- Builder.liftIRState $ gets Builder.builderBlock
  case pb'maybe of
    Nothing -> Builder.liftIRState $ modify \s ->
      s { Builder.builderBlock = Just $! Builder.emptyPartialBlock name }
    Just _ -> return ()

-- | make sure there is always an active block there.
--
-- A new block is created with numbered name if there is no active block.
ensureBlock :: Builder.MonadIRBuilder m => m ()
ensureBlock = Builder.ensureBlock
{-# INLINE ensureBlock #-}

-- | end current active block if there is one and create a new active block.
--
-- if current block has no terminator, append a "ret operand" or "ret" at end.
startNewBlock :: Builder.MonadIRBuilder m => Maybe IR.Operand -> m ()
startNewBlock val = ensureBlockEndWith (IR.Do (IR.Ret val [])) >> ensureBlock
{-# INLINE startNewBlock #-}

-- | end current active block with terminator if there is one and it has no terminator, then
-- it creates a new active block.
startNewBlockWith :: Builder.MonadIRBuilder m => IR.Named IR.Terminator -> m ()
startNewBlockWith end = ensureBlockEndWith end >> ensureBlock
{-# INLINE startNewBlockWith #-}

-- | end current active block if there is one and create a new active with provided name block.
--
-- if current active block has no terminator, append "ret operand" or "ret".
--
-- It returns current block name.
startNewNamedBlock :: Builder.MonadIRBuilder m => Maybe IR.Operand -> IR.Name -> m IR.Name
startNewNamedBlock val name = ensureBlockEndWith (IR.Do (IR.Ret val [])) >> ensureNamedBlock name $> name
{-# INLINE startNewNamedBlock #-}

-- | end current active block if there is one and create a new active with provided name block.
--
-- if current actie block has no terminator, use terminator provided.
--
-- It returns current block name.
startNewNamedBlockWith :: Builder.MonadIRBuilder m => IR.Named IR.Terminator -> IR.Name -> m IR.Name
startNewNamedBlockWith end name = ensureBlockEndWith end >> ensureNamedBlock name $> name
{-# INLINE startNewNamedBlockWith #-}

-- | get generated basicblocks and flush whole state.
--
-- After this, there is no
--
-- 1. basic blocks
-- 2. active block
-- 3. name suggestion
--
-- available and all information about used names are lost.
flushBasicBlocks :: Builder.MonadIRBuilder m => m [IR.BasicBlock]
flushBasicBlocks = flushBasicBlocksWith Builder.emptyIRBuilder
{-# INLINE flushBasicBlocks #-}

-- | get generated basicblocks and flush whole state with provided one.
--
-- If there is an active block, it is ensured to be ended with "ret".
flushBasicBlocksWith :: Builder.MonadIRBuilder m => Builder.IRBuilderState -> m [IR.BasicBlock]
flushBasicBlocksWith builder = do
  ensureBlockEnd Nothing
  blocks <- Builder.liftIRState . gets $ getSnocList . Builder.builderBlocks
  Builder.liftIRState $ put builder
  return blocks
