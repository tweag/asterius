{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Workarounds
  ( patchWritePtrArrayOp
  , maskUnknownCCallTargets
  ) where

import Asterius.Builtins
import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapT)
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Type.Reflection

patchWritePtrArrayOp :: Data a => a -> a
patchWritePtrArrayOp t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Block {..} -> t {bodys = V.fromList $ w $ V.toList bodys}
          where w (e0@Store {value = Unresolved {unresolvedSymbol = "stg_MUT_ARR_PTRS_DIRTY_info"}}:Store {bytes = 1}:es) =
                  e0 : w es
                w (e0:e1:es) = patchWritePtrArrayOp e0 : w (e1 : es)
                w es = patchWritePtrArrayOp es
        _ -> go
    _ -> go
  where
    go = gmapT patchWritePtrArrayOp t

maskUnknownCCallTargets :: Data a => a -> a
maskUnknownCCallTargets t =
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
    go = gmapT maskUnknownCCallTargets t
    has_call_target sym =
      "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName sym) ||
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
      ]
