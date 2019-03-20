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
  , setReturnTypes
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
  , pointerF64
  , pointerF32
  , loadI64
  , loadI32
  , loadI16
  , loadI8
  , loadF64
  , loadF32
  , storeI64
  , storeI32
  , storeI16
  , storeI8
  , storeF64
  , storeF32
  , call
  , call'
  , callImport
  , callImport'
  , callIndirect
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
  , convertSInt64ToFloat64
  , truncSFloat64ToInt64
  , roundupBytesToWords
  , addInt64
  , subInt64
  , mulInt64
  , divUInt64
  , gtUInt64
  , geUInt64
  , shlInt64
  , shrUInt64
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
  , orInt64
  , andInt32
  , orInt32
  , symbol
  , symbol'
  , constI32
  , constI64
  , constF64
  , baseReg
  , r1
  , currentNursery
  , hpAlloc
  , mainCapability
  ) where

import Asterius.Internals
import Asterius.Types
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
  { retTypes :: [ValueType]
  , paramBuf :: DList ValueType
  , paramNum, localNum, labelNum :: Int
  , exprBuf :: DList Expression
  }

initialEDSLState :: EDSLState
initialEDSLState =
  EDSLState
    { retTypes = []
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

bundleExpressions :: [ValueType] -> [Expression] -> Expression
bundleExpressions vts el =
  case el of
    [] -> Nop
    [e] -> e
    _ -> Block {name = mempty, bodys = el, blockReturnTypes = vts}

runEDSL :: [ValueType] -> EDSL () -> AsteriusFunction
runEDSL vts (EDSL m) =
  AsteriusFunction
    { functionType =
        FunctionType {paramTypes = fromDList paramBuf, returnTypes = retTypes}
    , body = bundleExpressions vts $ fromDList exprBuf
    }
  where
    EDSLState {..} = execState m initialEDSLState

data LVal = LVal
  { getLVal :: Expression
  , putLVal :: Expression -> EDSL ()
  }

setReturnTypes :: [ValueType] -> EDSL ()
setReturnTypes vts = EDSL $ modify' $ \s -> s {retTypes = vts}

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
          , offset = fromIntegral o
          , valueType = vt
          , ptr = wrapInt64 bp
          }
    , putLVal =
        \v ->
          emit $
          Store
            { bytes = b
            , offset = fromIntegral o
            , ptr = wrapInt64 bp
            , value = v
            , valueType = vt
            }
    }

pointerI64, pointerI32, pointerI16, pointerI8, pointerF64, pointerF32 ::
     Expression -> Int -> LVal
pointerI64 = pointer I64 8

pointerI32 = pointer I32 4

pointerI16 = pointer I32 2

pointerI8 = pointer I32 1

pointerF64 = pointer F64 8

pointerF32 = pointer F32 4

loadI64, loadI32, loadI16, loadI8, loadF64, loadF32 ::
     Expression -> Int -> Expression
loadI64 bp o = getLVal $ pointerI64 bp o

loadI32 bp o = getLVal $ pointerI32 bp o

loadI16 bp o = getLVal $ pointerI16 bp o

loadI8 bp o = getLVal $ pointerI8 bp o

loadF64 bp o = getLVal $ pointerF64 bp o

loadF32 bp o = getLVal $ pointerF32 bp o

storeI64, storeI32, storeI16, storeI8, storeF64, storeF32 ::
     Expression -> Int -> Expression -> EDSL ()
storeI64 bp o = putLVal $ pointerI64 bp o

storeI32 bp o = putLVal $ pointerI32 bp o

storeI16 bp o = putLVal $ pointerI16 bp o

storeI8 bp o = putLVal $ pointerI8 bp o

storeF64 bp o = putLVal $ pointerF64 bp o

storeF32 bp o = putLVal $ pointerF32 bp o

call :: AsteriusEntitySymbol -> [Expression] -> EDSL ()
call f xs = emit Call {target = f, operands = xs, callReturnTypes = []}

call' :: AsteriusEntitySymbol -> [Expression] -> ValueType -> EDSL Expression
call' f xs vt = do
  lr <- mutLocal vt
  putLVal lr Call {target = f, operands = xs, callReturnTypes = [vt]}
  pure $ getLVal lr

callImport :: SBS.ShortByteString -> [Expression] -> EDSL ()
callImport f xs =
  emit CallImport {target' = f, operands = xs, callImportReturnTypes = []}

callImport' ::
     SBS.ShortByteString -> [Expression] -> ValueType -> EDSL Expression
callImport' f xs vt = do
  lr <- mutLocal vt
  putLVal
    lr
    CallImport {target' = f, operands = xs, callImportReturnTypes = [vt]}
  pure $ getLVal lr

callIndirect :: Expression -> EDSL ()
callIndirect f =
  emit
    CallIndirect
      { indirectTarget = wrapInt64 f
      , operands = []
      , functionType = FunctionType {paramTypes = [], returnTypes = []}
      }

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

block', loop' :: [ValueType] -> (Label -> EDSL ()) -> EDSL ()
block' vts cont = do
  lbl <- newLabel
  es <- newScope $ cont lbl
  emit Block {name = unLabel lbl, bodys = fromDList es, blockReturnTypes = vts}

blockWithLabel :: [ValueType] -> Label -> EDSL () -> EDSL ()
blockWithLabel vts lbl m = do
  es <- newScope m
  emit Block {name = unLabel lbl, bodys = fromDList es, blockReturnTypes = vts}

loop' vts cont = do
  lbl <- newLabel
  es <- newScope $ cont lbl
  emit Loop {name = unLabel lbl, body = bundleExpressions vts $ fromDList es}

if' :: [ValueType] -> Expression -> EDSL () -> EDSL () -> EDSL ()
if' vts cond t f = do
  t_es <- newScope t
  f_es <- newScope f
  emit
    If
      { condition = cond
      , ifTrue = bundleExpressions vts $ fromDList t_es
      , ifFalse = Just $ bundleExpressions vts $ fromDList f_es
      }

break' :: Label -> Maybe Expression -> EDSL ()
break' (Label lbl) cond = emit Break {name = lbl, breakCondition = cond}

whileLoop :: [ValueType] -> Expression -> EDSL () -> EDSL ()
whileLoop vts cond body =
  loop' vts $ \lbl -> if' vts cond (body *> break' lbl Nothing) mempty

switchI64 :: Expression -> (EDSL () -> ([(Int, EDSL ())], EDSL ())) -> EDSL ()
switchI64 cond make_clauses =
  block' [] $ \switch_lbl ->
    let exit_switch = break' switch_lbl Nothing
        (clauses, def_clause) = make_clauses exit_switch
        switch_block = do
          switch_def_lbl <- newLabel
          blockWithLabel [] switch_def_lbl $ do
            clause_seq <-
              for (reverse clauses) $ \(clause_i, clause_m) -> do
                clause_lbl <- newLabel
                pure (clause_i, clause_lbl, clause_m)
            foldr
              (\(_, clause_lbl, clause_m) tot_m -> do
                 blockWithLabel [] clause_lbl tot_m
                 clause_m)
              (foldr
                 (\(clause_i, clause_lbl, _) br_m -> do
                    break' clause_lbl $ Just $ cond `eqInt64` constI64 clause_i
                    br_m)
                 (break' switch_def_lbl Nothing)
                 clause_seq)
              clause_seq
          def_clause
     in switch_block

notInt64, notInt32, eqZInt64, eqZInt32, extendUInt32, wrapInt64, convertUInt64ToFloat64, truncUFloat64ToInt64, convertSInt64ToFloat64, truncSFloat64ToInt64, roundupBytesToWords ::
     Expression -> Expression
notInt64 = eqZInt64

notInt32 = eqZInt32

eqZInt64 = Unary EqZInt64

eqZInt32 = Unary EqZInt32

extendUInt32 = Unary ExtendUInt32

wrapInt64 = Unary WrapInt64

convertUInt64ToFloat64 = Unary ConvertUInt64ToFloat64

truncUFloat64ToInt64 = Unary TruncUFloat64ToInt64

convertSInt64ToFloat64 = Unary ConvertSInt64ToFloat64

truncSFloat64ToInt64 = Unary TruncSFloat64ToInt64

roundupBytesToWords n = (n `addInt64` constI64 7) `divUInt64` constI64 8

addInt64, subInt64, mulInt64, divUInt64, gtUInt64, geUInt64, shlInt64, shrUInt64, geUInt32, addInt32, subInt32, mulInt32, eqInt64, eqInt32, ltUInt64, leUInt64, ltUInt32, neInt64, neInt32, andInt64, orInt64, andInt32, orInt32 ::
     Expression -> Expression -> Expression
addInt64 = Binary AddInt64

subInt64 = Binary SubInt64

mulInt64 = Binary MulInt64

divUInt64 = Binary DivUInt64

gtUInt64 = Binary GtUInt64

geUInt64 = Binary GeUInt64

shlInt64 = Binary ShlInt64

shrUInt64 = Binary ShrUInt64

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

orInt64 = Binary OrInt64

andInt32 = Binary AndInt32

orInt32 = Binary OrInt32

symbol :: AsteriusEntitySymbol -> Expression
symbol sym =
  Symbol {unresolvedSymbol = sym, symbolOffset = 0, resolvedSymbol = Nothing}

symbol' :: AsteriusEntitySymbol -> Int -> Expression
symbol' sym o =
  Symbol {unresolvedSymbol = sym, symbolOffset = o, resolvedSymbol = Nothing}

constI32, constI64, constF64 :: Int -> Expression
constI32 = ConstI32 . fromIntegral

constI64 = ConstI64 . fromIntegral

constF64 = ConstF64 . fromIntegral

baseReg, r1, currentNursery, hpAlloc :: LVal
baseReg = global BaseReg

r1 = global $ VanillaReg 1

currentNursery = global CurrentNursery

hpAlloc = global HpAlloc

mainCapability :: Expression
mainCapability = symbol "MainCapability"
