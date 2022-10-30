module Tlang.Codegen
where

import LLVM.AST as AST
import LLVM.AST.Global as Global
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.CallingConvention as CallingConvention
import LLVM.AST.Instruction
import LLVM.AST.Type (ptr)
import LLVM.AST.Name (Name (UnName))

import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad
import Data.List
import Data.Function

import qualified Data.ByteString.Short as ByteStringShort (toShort)
import qualified Data.ByteString.Char8 as ByteStringChar8 (pack)

data Symbol = Symbol
  { symRef :: String
  , symType :: Type
  , symVal :: Operand
  } deriving (Show, Eq)

type Names = Map.Map String Int

data CodegenState
  = CodegenState { currentBlock :: Name                     -- Name of the active block to append to
                 , blocks       :: Map.Map Name BlockState  -- Blocks for function
                 , symtab       :: [Symbol]                 -- Function scope symbol table
                 , blockCount   :: Int                      -- Count of basic blocks
                 , count        :: Word                     -- Count of unnamed instructions
                 , names        :: Names                    -- Name Supply
                 } deriving Show

data BlockState
  = BlockState { idx   :: Int                            -- Block index
               , stack :: [Named Instruction]            -- Stack of instructions
               , term  :: Maybe (Named Terminator)       -- Block terminator
               } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

createModule :: String -> AST.Module
createModule label = defaultModule { moduleName = ByteStringShort.toShort $ ByteStringChar8.pack label }

entryBlockName :: Name
entryBlockName = mkName "entry"

emptyBlockState :: Int -> BlockState
emptyBlockState i = BlockState i [] Nothing

execCodegen :: Codegen a -> (a, CodegenState)
execCodegen m = runState (runCodegen m) initCodegen
  where
    initCodegen :: CodegenState
    initCodegen = CodegenState entryBlockName Map.empty [] 1 0 Map.empty


-- Module Definition manipulation
appendDefinition :: Definition -> LLVM ()
appendDefinition d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

-- declare variable, function and alias
declare :: Global -> LLVM ()
declare = appendDefinition . GlobalDefinition

-- declare new type definition or opaque type
declareType :: Name -> Maybe Type -> LLVM ()
declareType name typ = appendDefinition $ TypeDefinition name typ

defineGlobalVars :: Either String Name -> Type -> Bool -> Maybe Constant.Constant -> LLVM ()
defineGlobalVars name'either typ isConstant val'maybe =  declare $
  globalVariableDefaults { name = either mkName id name'either
                         , Global.type' = typ
                         , isConstant = isConstant
                         , initializer = val'maybe
                         }

defineFunction :: Either String Name -> ([(Type, Name)], Type) -> [BasicBlock] -> LLVM ()
defineFunction name'either (paras, retyp) blocks = declare $
  functionDefaults {
    name = either mkName id name'either
  , returnType = retyp
  , parameters = ([Parameter ty n [] | (ty, n) <- paras], False)
  , basicBlocks = blocks
  }

-- declare a function signature, no basicblocks attached.
declareFunction :: Either String Name -> ([Type], Type) -> Maybe Linkage.Linkage -> LLVM ()
declareFunction name'either (typs, retyp) link'maybe = declare $
  functionDefaults {
    name = either mkName id name'either
  , returnType = retyp
  , parameters = ([Parameter ty n [] | (ty, n) <- zip typs $ fmap UnName [1 .. fromIntegral (length typs)]], False)
  , linkage = maybe Linkage.External id link'maybe
  }

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (name, (BlockState _ s t)) = BasicBlock name (reverse s) (maketerm t)
  where maketerm (Just x) = x
        maketerm Nothing = error $ "Block has no terminator: " ++ (show name)

transCodegenBlocks :: CodegenState -> [BasicBlock]
transCodegenBlocks m = fmap makeBlock . sortBlocks $ Map.toList (blocks m)
  where
    sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
    sortBlocks = sortBy (compare `on` (idx . snd))

-- Codegen Block Operations
codegenEntry :: Codegen Name
codegenEntry = gets currentBlock

-- add an empty block to blockState with unique name
addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names
  let (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (mkName qname) (emptyBlockState ix) bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (mkName qname)

-- generate unique name based on index in mapset
-- TODO: figure out better naming scheme
uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

switchBlock :: Name -> Codegen ()
switchBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return ()

-- add new empty block and switch to it
newBlock :: String -> Codegen Name
newBlock nm = do
  name <- addBlock nm
  switchBlock name
  return name

newBlock_ :: String -> Codegen ()
newBlock_ a = newBlock a >> return ()

selectedBlock :: Codegen Name
selectedBlock = gets currentBlock

current :: Codegen (Name, BlockState)
current = do
  name <- selectedBlock
  blks <- gets blocks
  case Map.lookup name blks of
    Just x -> return (name, x)
    Nothing -> error $ "No such block: " ++ show name

-- TODO: check existence of current block
modifyBlock :: (BlockState -> BlockState) -> Codegen ()
modifyBlock update = do
  (active, blk) <- current
  modify $ \s -> s { blocks = Map.insert active (update blk) (blocks s) }

-- instructions: Name management
--
-- return a freshed index
fresh :: Codegen Word
fresh = do
  i <- (1 +) <$> gets count
  modify $ \s -> s { count = i }
  pure i

globalNameRef :: Type -> Name -> Operand
globalNameRef t n = ConstantOperand $ Constant.GlobalReference t n
localNameRef :: Type -> Name -> Operand
localNameRef = LocalReference

assign :: String -> Type -> Operand -> Codegen ()
assign var typ x = do
  modify $ \s -> s { symtab = Symbol var typ x : symtab s }

getvar :: String -> Codegen (Type, Operand)
getvar var = do
  syms <- gets symtab
  case find ((== var) . symRef) syms of
    Just x  -> return (symType x, symVal x)
    Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Type -> Instruction -> Codegen (Type, Operand)
instr typ ins = do
  ref <- UnName <$> fresh
  insts <- stack . snd <$> current
  modifyBlock (\blk -> blk { stack = (ref := ins) : insts } )
  return $ (typ, localNameRef typ ref)

instr' :: Instruction -> Codegen ()
instr' ins = do
  insts <- stack . snd <$> current
  modifyBlock $ \blk -> blk { stack = Do ins : insts }
  return ()

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator end = do
  modifyBlock (\blk -> blk { term = Just end })
  return end

-- arithmetic operation for floating number
fadd, fsub, fmul, fdiv :: Type -> Operand -> Operand -> Codegen (Type, Operand)
fadd typ a b = instr typ $ FAdd noFastMathFlags a b []
fsub typ a b = instr typ $ FSub noFastMathFlags a b []
fmul typ a b = instr typ $ FMul noFastMathFlags a b []
fdiv typ a b = instr typ $ FDiv noFastMathFlags a b []

add, sub, mul :: Bool -> Bool -> Type -> Operand -> Operand -> Codegen (Type, Operand)
add nsw nuw typ a b = instr typ $ Add nsw nuw a b []
sub nsw nuw typ a b = instr typ $ Sub nsw nuw a b []
mul nsw nuw typ a b = instr typ $ Mul nsw nuw a b []

udiv, sdiv :: Bool -> Type -> Operand -> Operand -> Codegen (Type, Operand)
udiv exact typ a b = instr typ $ UDiv exact a b []
sdiv exact typ a b = instr typ $ SDiv exact a b []

bitcast :: Type -> Operand -> Codegen (Type, Operand)
bitcast typ v = instr typ $ BitCast v typ []

phi :: Type -> [(Operand, Name)] -> Codegen (Type, Operand)
phi typ vs = instr typ $ Phi typ vs []

-- Control instruction
br :: Name -> Codegen (Named Terminator)
br label = terminator $ Do $ Br label []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Maybe Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret val []

call :: Type -> Operand -> [Operand] -> Codegen (Type, Operand)
call typ fn args = instr typ $ Call Nothing CallingConvention.C [] (Right fn) ((,[]) <$> args) [] []

-- allocate memory on program execution stack
alloca :: Type -> Codegen (Type, Operand)
alloca ty = instr (ptr ty) $ Alloca ty Nothing 0 []

-- store value to a pointer
store :: Operand -> Operand -> Codegen ()
store addr val = instr' $ Store False addr val Nothing 0 []

-- read value from a pointer
load :: Type -> Operand -> Codegen (Type, Operand)
load typ addr = instr typ $ Load False addr Nothing 0 []
