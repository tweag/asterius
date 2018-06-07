{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Ostrich
  ( ostrich
  ) where

import Asterius.Builtins
import Asterius.Types
import Data.Data (Data, gmapT)
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Type.Reflection

ostrich :: Data a => a -> a
ostrich t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Call {..}
          | not $ has_call_target target ->
            marshalErrorCode errUnimplemented valueType
          | target == "createIOThread" ->
            case V.toList operands of
              [cap, stack_size_w@Load {valueType = I32}, target_closure] ->
                t
                  { operands =
                      [ cap
                      , Unary {unaryOp = ExtendUInt32, operand0 = stack_size_w}
                      , target_closure
                      ]
                  }
              _ -> t
          | otherwise -> t
        AtomicRMW {} -> marshalErrorCode errAtomics I64
        AtomicLoad {} -> marshalErrorCode errAtomics I64
        AtomicStore {} -> marshalErrorCode errAtomics None
        AtomicCmpxchg {} -> marshalErrorCode errAtomics I64
        _ -> go
    _ -> go
  where
    go = gmapT ostrich t
    has_call_target sym =
      sym `HS.member`
      [ "main"
      , "init_rts_asterius"
      , "rts_evalIO"
      , "scheduleWaitThread"
      , "createThread"
      , "createGenThread"
      , "createIOThread"
      , "createStrictIOThread"
      , "allocate"
      , "allocateMightFail"
      , "allocatePinned"
      , "allocBlock"
      , "allocBlock_lock"
      , "allocBlockOnNode"
      , "allocBlockOnNode_lock"
      , "allocGroup"
      , "allocGroup_lock"
      , "allocGroupOnNode"
      , "allocGroupOnNode_lock"
      , "free"
      , "newCAF"
      , "StgRun"
      , "StgReturn"
      , "print_i64"
      , "print_f32"
      , "print_f64"
      , "_get_Sp"
      , "_get_SpLim"
      , "_get_Hp"
      , "_get_HpLim"
      ]
