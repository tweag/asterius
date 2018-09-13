{-# LANGUAGE GADTs #-}
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
import qualified Data.Map.Strict as M
import Type.Reflection

patchWritePtrArrayOp :: Data a => a -> a
patchWritePtrArrayOp t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Block {..} -> t {bodys = w bodys}
          where w (e0@Store {value = Unresolved {unresolvedSymbol = "stg_MUT_ARR_PTRS_DIRTY_info"}}:Store {bytes = 1}:es) =
                  e0 : w es
                w (e0:e1:es) = patchWritePtrArrayOp e0 : w (e1 : es)
                w es = patchWritePtrArrayOp es
        _ -> go
    _ -> go
  where
    go = gmapT patchWritePtrArrayOp t

maskUnknownCCallTargets :: Data a => [AsteriusEntitySymbol] -> a -> a
maskUnknownCCallTargets export_funcs = f
  where
    has_call_target sym =
      "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName sym) ||
      sym `M.member`
      functionMap (rtsAsteriusModule unsafeDefaultBuiltinsOptions) ||
      sym `elem` export_funcs
    f :: Data a => a -> a
    f t =
      let go = gmapT f t
       in case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
            Just HRefl ->
              case t of
                Call {..}
                  | not $ has_call_target target ->
                    marshalErrorCode errUnimplemented valueType
                  | target == "createIOThread" ->
                    case operands of
                      [cap, stack_size_w@Load {valueType = I32}, target_closure] ->
                        t
                          { operands =
                              [ cap
                              , Unary
                                  { unaryOp = ExtendUInt32
                                  , operand0 = stack_size_w
                                  }
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
