{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.EDSL
  ( EDSL
  , emit
  , runEDSL
  , LVal
  , getLVal
  , putLVal
  , setReturnType
  , mutParam
  , mutLocal
  , param
  , params
  , local
  , i64Local
  , i32Local
  , i64MutLocal
  , i32MutLocal
  , global
  , pointer
  , pointerI64
  , pointerI32
  , pointerI16
  , pointerI8
  , loadI64
  , loadI32
  , loadI16
  , loadI8
  , storeI64
  , storeI32
  , storeI16
  , storeI8
  , call
  , call'
  , callImport
  , callImport'
  , callIndirect'
  , Label
  , block'
  , loop'
  , if'
  , break'
  , whileLoop
  , switchI64
  , notInt64
  , notInt32
  , eqZInt64
  , eqZInt32
  , extendUInt32
  , wrapInt64
  , convertUInt64ToFloat64
  , truncUFloat64ToInt64
  , growMemory
  , roundupBytesToWords
  , addInt64
  , subInt64
  , mulInt64
  , divUInt64
  , gtUInt64
  , geUInt64
  , geUInt32
  , addInt32
  , subInt32
  , mulInt32
  , eqInt64
  , eqInt32
  , ltUInt64
  , leUInt64
  , ltUInt32
  , neInt64
  , neInt32
  , andInt64
  , andInt32
  , orInt32
  , symbol
  , constI32
  , constI64
  , baseReg
  , r1
  , sp
  , spLim
  , hp
  , hpLim
  , cccs
  , currentTSO
  , currentNursery
  , hpAlloc
  , mainCapability
  , endTSOQueue
  ) where

import Asterius.Internals
import Asterius.Types
import Asterius.TypesConv
import Control.Monad.Fail
import Control.Monad.State.Strict
import qualified Data.ByteString.Short as SBS
import Data.Monoid
import Data.Traversable

type DList a = Endo [a]

dListSnoc :: DList a -> a -> DList a
dListSnoc dl a = dl <> Endo (a :)

fromDList :: DList a -> [a]
fromDList = ($ []) . appEndo

data EDSLState = EDSLState
  { retType :: ValueType
  , paramBuf :: DList ValueType
  , paramNum, localNum, labelNum :: Int
  , exprBuf :: DList Expression
  }

initialEDSLState :: EDSLState
initialEDSLState =
  EDSLState
    { retType = None
    , paramBuf = mempty
    , paramNum = 0
    , localNum = 0
    , labelNum = 0
    , exprBuf = mempty
    }

newtype EDSL a =
  EDSL (State EDSLState a)
  deriving (Functor, Applicative, Monad)

instance MonadFail EDSL where
  fail = pure . error

instance Semigroup a => Semigroup (EDSL a) where
  p0 <> p1 = (<>) <$> p0 <*> p1

instance Monoid a => Monoid (EDSL a) where
  mempty = pure mempty

emit :: Expression -> EDSL ()
emit e =
  EDSL $ modify' $ \s@EDSLState {..} -> s {exprBuf = exprBuf `dListSnoc` e}

bundleExpressions :: [Expression] -> Expression
bundleExpressions el =
  case el of
    [] -> Nop
    [e] -> e
    _ -> Block {name = mempty, bodys = el, valueType = Auto}

runEDSL :: EDSL () -> AsteriusFunction
runEDSL (EDSL m) =
  AsteriusFunction
    { functionType =
        FunctionType {returnType = retType, paramTypes = fromDList paramBuf}
    , body = bundleExpressions $ fromDList exprBuf
    }
  where
    EDSLState {..} = execState m initialEDSLState

data LVal = LVal
  { getLVal :: Expression
  , putLVal :: Expression -> EDSL ()
  }

setReturnType :: ValueType -> EDSL ()
setReturnType vt = EDSL $ modify' $ \s -> s {retType = vt}

mutParam, mutLocal :: ValueType -> EDSL LVal
mutParam vt =
  EDSL $ do
    i <-
      state $ \s@EDSLState {..} ->
        ( fromIntegral paramNum
        , s {paramBuf = paramBuf `dListSnoc` vt, paramNum = succ paramNum})
    pure
      LVal
        { getLVal = GetLocal {index = i, valueType = vt}
        , putLVal = \v -> emit SetLocal {index = i, value = v}
        }

mutLocal vt =
  EDSL $ do
    i <- state $ \s@EDSLState {..} -> (localNum, s {localNum = succ localNum})
    let lr = UniqueLocalReg i vt
    pure
      LVal
        { getLVal = UnresolvedGetLocal {unresolvedLocalReg = lr}
        , putLVal =
            \v -> emit UnresolvedSetLocal {unresolvedLocalReg = lr, value = v}
        }

param :: ValueType -> EDSL Expression
param vt = do
  p <- mutParam vt
  pure $ getLVal p

params :: [ValueType] -> EDSL [Expression]
params vt = for vt param

local :: ValueType -> Expression -> EDSL Expression
local vt v = do
  lr <- mutLocal vt
  putLVal lr v
  pure $ getLVal lr

i64Local, i32Local :: Expression -> EDSL Expression
i64Local = local I64

i32Local = local I32

i64MutLocal, i32MutLocal :: EDSL LVal
i64MutLocal = mutLocal I64

i32MutLocal = mutLocal I32

global :: UnresolvedGlobalReg -> LVal
global gr =
  LVal
    { getLVal = UnresolvedGetGlobal {unresolvedGlobalReg = gr}
    , putLVal =
        \v -> emit $ UnresolvedSetGlobal {unresolvedGlobalReg = gr, value = v}
    }

pointer :: ValueType -> BinaryenIndex -> Expression -> Int -> LVal
pointer vt b bp o =
  LVal
    { getLVal =
        Load
          { signed = False
          , bytes = b
          , offset = 0
          , align = 0
          , valueType = vt
          , ptr = p
          }
    , putLVal =
        \v ->
          emit $
          Store
            { bytes = b
            , offset = 0
            , align = 0
            , ptr = p
            , value = v
            , valueType = vt
            }
    }
  where
    p =
      wrapInt64 $
      case o of
        0 -> bp
        _ -> bp `addInt64` constI64 o

pointerI64, pointerI32, pointerI16, pointerI8 :: Expression -> Int -> LVal
pointerI64 = pointer I64 8

pointerI32 = pointer I32 4

pointerI16 = pointer I32 2

pointerI8 = pointer I32 1

loadI64, loadI32, loadI16, loadI8 :: Expression -> Int -> Expression
loadI64 bp o = getLVal $ pointerI64 bp o

loadI32 bp o = getLVal $ pointerI32 bp o

loadI16 bp o = getLVal $ pointerI16 bp o

loadI8 bp o = getLVal $ pointerI8 bp o

storeI64, storeI32, storeI16, storeI8 ::
     Expression -> Int -> Expression -> EDSL ()
storeI64 bp o = putLVal $ pointerI64 bp o

storeI32 bp o = putLVal $ pointerI32 bp o

storeI16 bp o = putLVal $ pointerI16 bp o

storeI8 bp o = putLVal $ pointerI8 bp o

call :: AsteriusEntitySymbol -> [Expression] -> EDSL ()
call f xs = emit Call {target = f, operands = xs, valueType = None}

call' :: AsteriusEntitySymbol -> [Expression] -> ValueType -> EDSL Expression
call' f xs vt = do
  lr <- mutLocal vt
  putLVal lr Call {target = f, operands = xs, valueType = vt}
  pure $ getLVal lr

callImport :: SBS.ShortByteString -> [Expression] -> EDSL ()
callImport f xs = emit CallImport {target' = f, operands = xs, valueType = None}

callImport' ::
     SBS.ShortByteString -> [Expression] -> ValueType -> EDSL Expression
callImport' f xs vt = do
  lr <- mutLocal vt
  putLVal lr CallImport {target' = f, operands = xs, valueType = vt}
  pure $ getLVal lr

callIndirect' :: Expression -> [Expression] -> FunctionType -> EDSL Expression
callIndirect' f xs ft = do
  lr <- mutLocal (returnType ft)
  putLVal
    lr
    CallIndirect
      { indirectTarget = wrapInt64 f
      , operands = xs
      , typeName = generateWasmFunctionTypeName ft
      }
  pure $ getLVal lr

newtype Label = Label
  { unLabel :: SBS.ShortByteString
  }

newLabel :: EDSL Label
newLabel =
  EDSL $
  state $ \s@EDSLState {..} ->
    (Label $ showSBS labelNum, s {labelNum = succ labelNum})

newScope :: EDSL () -> EDSL (DList Expression)
newScope m = do
  orig_buf <-
    EDSL $ state $ \s@EDSLState {..} -> (exprBuf, s {exprBuf = mempty})
  m
  EDSL $ state $ \s@EDSLState {..} -> (exprBuf, s {exprBuf = orig_buf})

block', loop' :: (Label -> EDSL ()) -> EDSL ()
block' cont = do
  lbl <- newLabel
  es <- newScope $ cont lbl
  emit Block {name = unLabel lbl, bodys = fromDList es, valueType = Auto}

blockWithLabel :: Label -> EDSL () -> EDSL ()
blockWithLabel lbl m = do
  es <- newScope m
  emit Block {name = unLabel lbl, bodys = fromDList es, valueType = Auto}

loop' cont = do
  lbl <- newLabel
  es <- newScope $ cont lbl
  emit Loop {name = unLabel lbl, body = bundleExpressions $ fromDList es}

if' :: Expression -> EDSL () -> EDSL () -> EDSL ()
if' cond t f = do
  t_es <- newScope t
  f_es <- newScope f
  emit
    If
      { condition = cond
      , ifTrue = bundleExpressions $ fromDList t_es
      , ifFalse = bundleExpressions $ fromDList f_es
      }

break' :: Label -> Expression -> EDSL ()
break' (Label lbl) cond =
  emit Break {name = lbl, condition = cond, value = Null}

whileLoop :: Expression -> EDSL () -> EDSL ()
whileLoop cond body = loop' $ \lbl -> if' cond (body *> break' lbl Null) mempty

switchI64 :: Expression -> (EDSL () -> ([(Int, EDSL ())], EDSL ())) -> EDSL ()
switchI64 cond make_clauses =
  block' $ \switch_lbl ->
    let exit_switch = break' switch_lbl Null
        (clauses, def_clause) = make_clauses exit_switch
        switch_block = do
          switch_def_lbl <- newLabel
          blockWithLabel switch_def_lbl $ do
            clause_seq <-
              for (reverse clauses) $ \(clause_i, clause_m) -> do
                clause_lbl <- newLabel
                pure (clause_i, clause_lbl, clause_m)
            foldr
              (\(_, clause_lbl, clause_m) tot_m -> do
                 blockWithLabel clause_lbl tot_m
                 clause_m)
              (foldr
                 (\(clause_i, clause_lbl, _) br_m -> do
                    break' clause_lbl $ cond `eqInt64` constI64 clause_i
                    br_m)
                 (break' switch_def_lbl Null)
                 clause_seq)
              clause_seq
          def_clause
     in switch_block

notInt64, notInt32, eqZInt64, eqZInt32, extendUInt32, wrapInt64, convertUInt64ToFloat64, truncUFloat64ToInt64, growMemory, roundupBytesToWords ::
     Expression -> Expression
notInt64 = eqZInt64

notInt32 = eqZInt32

eqZInt64 = Unary EqZInt64

eqZInt32 = Unary EqZInt32

extendUInt32 = Unary ExtendUInt32

wrapInt64 = Unary WrapInt64

convertUInt64ToFloat64 = Unary ConvertUInt64ToFloat64

truncUFloat64ToInt64 = Unary TruncUFloat64ToInt64

growMemory x = Host {hostOp = GrowMemory, name = "", operands = [x]}

roundupBytesToWords n = (n `addInt64` constI64 7) `divUInt64` constI64 8

addInt64, subInt64, mulInt64, divUInt64, gtUInt64, geUInt64, geUInt32, addInt32, subInt32, mulInt32, eqInt64, eqInt32, ltUInt64, leUInt64, ltUInt32, neInt64, neInt32, andInt64, andInt32, orInt32 ::
     Expression -> Expression -> Expression
addInt64 = Binary AddInt64

subInt64 = Binary SubInt64

mulInt64 = Binary MulInt64

divUInt64 = Binary DivUInt64

gtUInt64 = Binary GtUInt64

geUInt64 = Binary GeUInt64

geUInt32 = Binary GeUInt32

addInt32 = Binary AddInt32

subInt32 = Binary SubInt32

mulInt32 = Binary MulInt32

eqInt64 = Binary EqInt64

eqInt32 = Binary EqInt32

ltUInt64 = Binary LtUInt64

leUInt64 = Binary LeUInt64

ltUInt32 = Binary LtUInt32

neInt64 = Binary NeInt64

neInt32 = Binary NeInt32

andInt64 = Binary AndInt64

andInt32 = Binary AndInt32

orInt32 = Binary OrInt32

symbol :: AsteriusEntitySymbol -> Expression
symbol = Unresolved

constI32, constI64 :: Int -> Expression
constI32 = ConstI32 . fromIntegral

constI64 = ConstI64 . fromIntegral

baseReg, r1, sp, spLim, hp, hpLim, cccs, currentTSO, currentNursery, hpAlloc ::
     LVal
baseReg = global BaseReg

r1 = global $ VanillaReg 1

sp = global Sp

spLim = global SpLim

hp = global Hp

hpLim = global HpLim

cccs = global CCCS

currentTSO = global CurrentTSO

currentNursery = global CurrentNursery

hpAlloc = global HpAlloc

mainCapability, endTSOQueue :: Expression
mainCapability = symbol "MainCapability"

endTSOQueue = symbol "stg_END_TSO_QUEUE_closure"
