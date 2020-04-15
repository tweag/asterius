-----------------------------------------------------------------------------
-- |
-- Module      :  Asterius.EDSL
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Embedded DSL for creating 'AsteriusModule's.
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.EDSL
  ( EDSL,
    emit,
    runEDSL,
    LVal,
    getLVal,
    putLVal,
    setReturnTypes,
    mutParam,
    mutLocal,
    param,
    params,
    local,
    i64Local,
    i32Local,
    i64MutLocal,
    global,
    pointer,
    pointerI64,
    pointerI32,
    pointerI16,
    pointerI8,
    pointerF64,
    pointerF32,
    loadI64,
    loadI32,
    loadI16,
    loadI8,
    loadF64,
    loadF32,
    storeI64,
    storeI32,
    storeI16,
    storeI8,
    storeF64,
    storeF32,
    unTagClosure,
    call,
    call',
    callImport,
    callImport',
    callIndirect,
    Label,
    block',
    loop',
    if',
    break',
    whileLoop,
    switchI64,
    module Asterius.EDSL.BinaryOp,
    module Asterius.EDSL.UnaryOp,
    notInt64,
    nandInt64,
    symbol,
    symbol',
    constI32,
    constI64,
    constF64,
    baseReg,
    r1,
    currentNursery,
    currentTSO,
    currentTID,
    hpAlloc,
    mainCapability,
  )
where

import Asterius.EDSL.BinaryOp
import Asterius.EDSL.UnaryOp
import Asterius.Internals
import Asterius.Passes.All
import Asterius.Passes.Barf
import Asterius.Passes.GlobalRegs
import Asterius.Types
import Asterius.Types.EntitySymbolMap
import Bag
import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import Data.Traversable
import Language.Haskell.GHC.Toolkit.Constants

-- | State maintained by the EDSL builder.
data EDSLState
  = EDSLState
      { retTypes :: [ValueType],
        paramBuf :: Bag ValueType,
        paramNum :: Int,
        localNum :: Int,
        labelNum :: Int,
        exprBuf :: Bag Expression,
        -- | Static variables to be added into the module
        staticsBuf :: [(EntitySymbol, AsteriusStatics)]
      }

initialEDSLState :: EDSLState
initialEDSLState = EDSLState
  { retTypes = [],
    paramBuf = emptyBag,
    paramNum = 0,
    localNum = 0,
    labelNum = 0,
    exprBuf = emptyBag,
    staticsBuf = mempty
  }

newtype EDSL a
  = EDSL (State EDSLState a)
  deriving (Functor, Applicative, Monad)

instance MonadFail EDSL where
  fail = pure . error

instance Semigroup a => Semigroup (EDSL a) where
  p0 <> p1 = (<>) <$> p0 <*> p1

instance Monoid a => Monoid (EDSL a) where
  mempty = pure mempty

emit :: Expression -> EDSL ()
emit e =
  EDSL $ modify' $ \s@EDSLState {..} -> s {exprBuf = exprBuf `snocBag` e}

--  | Create a block from the list of expressions returning the given values.
bundleExpressions ::
  -- | Return values of the block
  [ValueType] ->
  -- | Expressions in the block
  [Expression] ->
  Expression
bundleExpressions vts el = case el of
  [] -> Nop
  [e] -> e
  _ -> Block {name = mempty, bodys = el, blockReturnTypes = vts}

-- | Build a module containing the function and some auxiliary data
-- given its name and a builder.
runEDSL ::
  -- | Function name
  EntitySymbol ->
  -- | Builder
  EDSL () ->
  -- | Final module
  AsteriusModule
runEDSL n (EDSL m) =
  m1
    { staticsMap = fromListESM staticsBuf <> staticsMap m1
    }
  where
    EDSLState {..} = execState m initialEDSLState
    f0 = adjustLocalRegs $ Function
      { functionType = FunctionType
          { paramTypes = bagToList paramBuf,
            returnTypes = retTypes
          },
        varTypes = [],
        body = bundleExpressions retTypes $ bagToList exprBuf
      }
    m1 = processBarf n f0

-- | Any value that can be read from and written to is an LVal.
data LVal
  = LVal
      { -- | Read from the LVal.
        getLVal :: Expression,
        -- | Write into the LVal.
        putLVal :: Expression -> EDSL ()
      }

-- | Set the return type of the EDSL expression.
setReturnTypes :: [ValueType] -> EDSL ()
setReturnTypes vts = EDSL $ modify' $ \s -> s {retTypes = vts}

mutParam :: ValueType -> EDSL LVal
mutParam vt = EDSL $ do
  i <- state $ \s@EDSLState {..} ->
    ( fromIntegral paramNum,
      s {paramBuf = paramBuf `snocBag` vt, paramNum = succ paramNum}
    )
  pure LVal
    { getLVal = GetLocal {index = i, valueType = vt},
      putLVal = \v -> emit SetLocal {index = i, value = v}
    }

mutLocal :: ValueType -> EDSL LVal
mutLocal vt = EDSL $ do
  i <- state $ \s@EDSLState {..} -> (localNum, s {localNum = succ localNum})
  let lr = UniqueLocalReg i vt
  pure LVal
    { getLVal = UnresolvedGetLocal {unresolvedLocalReg = lr},
      putLVal = \v ->
        emit UnresolvedSetLocal {unresolvedLocalReg = lr, value = v}
    }

param :: ValueType -> EDSL Expression
param vt = getLVal <$> mutParam vt

params :: [ValueType] -> EDSL [Expression]
params vt = for vt param

local :: ValueType -> Expression -> EDSL Expression
local vt v = do
  lr <- mutLocal vt
  putLVal lr v
  pure $ getLVal lr

i32Local :: Expression -> EDSL Expression
i32Local = local I32

i64Local :: Expression -> EDSL Expression
i64Local = local I64

i64MutLocal :: EDSL LVal
i64MutLocal = mutLocal I64

global :: UnresolvedGlobalReg -> LVal
global gr = LVal
  { getLVal = unresolvedGetGlobal gr,
    putLVal = emit . unresolvedSetGlobal gr
  }

pointer :: ValueType -> BinaryenIndex -> Expression -> Int -> LVal
pointer vt b bp o = LVal
  { getLVal = Load
      { signed = False,
        bytes = b,
        offset = fromIntegral o,
        valueType = vt,
        ptr = wrapInt64 bp
      },
    putLVal = \v -> emit $ Store
      { bytes = b,
        offset = fromIntegral o,
        ptr = wrapInt64 bp,
        value = v,
        valueType = vt
      }
  }

pointerI64 :: Expression -> Int -> LVal
pointerI64 = pointer I64 8

pointerI32 :: Expression -> Int -> LVal
pointerI32 = pointer I32 4

pointerI16 :: Expression -> Int -> LVal
pointerI16 = pointer I32 2

pointerI8 :: Expression -> Int -> LVal
pointerI8 = pointer I32 1

pointerF64 :: Expression -> Int -> LVal
pointerF64 = pointer F64 8

pointerF32 :: Expression -> Int -> LVal
pointerF32 = pointer F32 4

loadI64 :: Expression -> Int -> Expression
loadI64 bp o = getLVal $ pointerI64 bp o

loadI32 :: Expression -> Int -> Expression
loadI32 bp o = getLVal $ pointerI32 bp o

loadI16 :: Expression -> Int -> Expression
loadI16 bp o = getLVal $ pointerI16 bp o

loadI8 :: Expression -> Int -> Expression
loadI8 bp o = getLVal $ pointerI8 bp o

loadF64 :: Expression -> Int -> Expression
loadF64 bp o = getLVal $ pointerF64 bp o

loadF32 :: Expression -> Int -> Expression
loadF32 bp o = getLVal $ pointerF32 bp o

storeI64 :: Expression -> Int -> Expression -> EDSL ()
storeI64 bp o = putLVal $ pointerI64 bp o

storeI32 :: Expression -> Int -> Expression -> EDSL ()
storeI32 bp o = putLVal $ pointerI32 bp o

storeI16 :: Expression -> Int -> Expression -> EDSL ()
storeI16 bp o = putLVal $ pointerI16 bp o

storeI8 :: Expression -> Int -> Expression -> EDSL ()
storeI8 bp o = putLVal $ pointerI8 bp o

storeF64 :: Expression -> Int -> Expression -> EDSL ()
storeF64 bp o = putLVal $ pointerF64 bp o

storeF32 :: Expression -> Int -> Expression -> EDSL ()
storeF32 bp o = putLVal $ pointerF32 bp o

-- | Encode not using xor.
notInt64 :: Expression -> Expression
notInt64 e = e `xorInt64` constI64 0xFFFFFFFFFFFFFFFF

-- | Encode a nand using a not and an and.
nandInt64 :: Expression -> Expression -> Expression
nandInt64 e1 e2 = notInt64 $ andInt64 e1 e2

unTagClosure :: Expression -> Expression
unTagClosure p = p `andInt64` constI64 0xFFFFFFFFFFFFFFF8

call :: EntitySymbol -> [Expression] -> EDSL ()
call f xs = emit Call {target = f, operands = xs, callReturnTypes = []}

call' :: EntitySymbol -> [Expression] -> ValueType -> EDSL Expression
call' f xs vt = do
  lr <- mutLocal vt
  putLVal lr Call {target = f, operands = xs, callReturnTypes = [vt]}
  pure $ getLVal lr

-- | Call a function with no return value
callImport ::
  -- | Function name
  BS.ByteString ->
  -- | Parameter list
  [Expression] ->
  EDSL ()
callImport f xs =
  emit CallImport {target' = f, operands = xs, callImportReturnTypes = []}

-- | Call a function with a return value
callImport' ::
  -- | Function name
  BS.ByteString ->
  -- | Arguments
  [Expression] ->
  -- | Return type of function
  ValueType ->
  EDSL Expression
callImport' f xs vt = do
  lr <- mutLocal vt
  putLVal
    lr
    CallImport {target' = f, operands = xs, callImportReturnTypes = [vt]}
  pure $ getLVal lr

callIndirect :: Expression -> EDSL ()
callIndirect f = emit CallIndirect
  { indirectTarget = wrapInt64 f,
    operands = [],
    functionType = FunctionType {paramTypes = [], returnTypes = []}
  }

newtype Label
  = Label
      { unLabel :: BS.ByteString
      }

newLabel :: EDSL Label
newLabel = EDSL $ state $ \s@EDSLState {..} ->
  (Label $ showBS labelNum, s {labelNum = succ labelNum})

newScope :: EDSL () -> EDSL (Bag Expression)
newScope m = do
  orig_buf <- EDSL $ state $ \s@EDSLState {..} ->
    (exprBuf, s {exprBuf = emptyBag})
  m
  EDSL $ state $ \s@EDSLState {..} -> (exprBuf, s {exprBuf = orig_buf})

block' :: [ValueType] -> (Label -> EDSL ()) -> EDSL ()
block' vts cont = do
  lbl <- newLabel
  es <- newScope $ cont lbl
  emit Block
    { name = unLabel lbl,
      bodys = bagToList es,
      blockReturnTypes = vts
    }

blockWithLabel :: [ValueType] -> Label -> EDSL () -> EDSL ()
blockWithLabel vts lbl m = do
  es <- newScope m
  emit Block
    { name = unLabel lbl,
      bodys = bagToList es,
      blockReturnTypes = vts
    }

loop' :: [ValueType] -> (Label -> EDSL ()) -> EDSL ()
loop' vts cont = do
  lbl <- newLabel
  es <- newScope $ cont lbl
  emit Loop {name = unLabel lbl, body = bundleExpressions vts $ bagToList es}

if' :: [ValueType] -> Expression -> EDSL () -> EDSL () -> EDSL ()
if' vts cond t f = do
  t_es <- newScope t
  f_es <- newScope f
  emit If
    { condition = cond,
      ifTrue = bundleExpressions vts $ bagToList t_es,
      ifFalse = Just $ bundleExpressions vts $ bagToList f_es
    }

break' :: Label -> Maybe Expression -> EDSL ()
break' (Label lbl) cond = emit Break {name = lbl, breakCondition = cond}

whileLoop :: Expression -> EDSL () -> EDSL ()
whileLoop cond body =
  loop' [] $ \lbl -> if' [] cond (body *> break' lbl Nothing) mempty

switchI64 :: Expression -> (EDSL () -> ([(Int, EDSL ())], EDSL ())) -> EDSL ()
switchI64 cond make_clauses = block' [] $ \switch_lbl ->
  let exit_switch = break' switch_lbl Nothing
      (clauses, def_clause) = make_clauses exit_switch
      switch_block = do
        switch_def_lbl <- newLabel
        blockWithLabel [] switch_def_lbl $ do
          clause_seq <- for (reverse clauses) $ \(clause_i, clause_m) -> do
            clause_lbl <- newLabel
            pure (clause_i, clause_lbl, clause_m)
          foldr
            ( \(_, clause_lbl, clause_m) tot_m -> do
                blockWithLabel [] clause_lbl tot_m
                clause_m
            )
            ( foldr
                ( \(clause_i, clause_lbl, _) br_m -> do
                    break' clause_lbl $ Just $ cond `eqInt64` constI64 clause_i
                    br_m
                )
                (break' switch_def_lbl Nothing)
                clause_seq
            )
            clause_seq
        def_clause
   in switch_block

-- | Allocate a static region of bytes in the global section. Returns a
-- reference to the variable (symbol). Usage:
--
-- >  runEDSL $ do
-- >    x <- allocStaticBytes "x"
-- >          (Serialized $ BS.pack $ replicate 8 1)
-- >    loadi64 x 0
-- >
-- >    y <- allocStaticBytes "y" (Uninitialized 8)
-- >    storei64 x 0 (constI32 32)
allocStaticBytes ::
  -- | Name of the static region
  EntitySymbol ->
  -- | Initializer
  AsteriusStatic ->
  -- | Expression to access the static, referenced by name.
  EDSL Expression
allocStaticBytes n v = EDSL $ state $ \st ->
  let st' =
        st
          { staticsBuf =
              (n, AsteriusStatics {staticsType = Bytes, asteriusStatics = [v]})
                : staticsBuf st
          }
   in (symbol n, st')

symbol :: EntitySymbol -> Expression
symbol = flip symbol' 0

symbol' :: EntitySymbol -> Int -> Expression
symbol' sym o = Symbol {unresolvedSymbol = sym, symbolOffset = o}

constI32 :: Int -> Expression
constI32 = ConstI32 . fromIntegral

constI64 :: Int -> Expression
constI64 = ConstI64 . fromIntegral

constF64 :: Int -> Expression
constF64 = ConstF64 . fromIntegral

baseReg :: LVal
baseReg = global BaseReg

r1 :: LVal
r1 = global $ VanillaReg 1

currentNursery :: LVal
currentNursery = global CurrentNursery

currentTSO :: LVal
currentTSO = global CurrentTSO

hpAlloc :: LVal
hpAlloc = global HpAlloc

mainCapability :: Expression
mainCapability = symbol "MainCapability"

currentTID :: Expression
currentTID = loadI32 (getLVal currentTSO) offset_StgTSO_id
