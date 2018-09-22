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
import Data.Data (Data, gmapM)
import qualified Data.Map.Strict as M
import Type.Reflection

patchWritePtrArrayOp :: (Monad m, Data a) => a -> m a
patchWritePtrArrayOp t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Block {..} -> do
          new_bodys <- w bodys
          pure t {bodys = new_bodys}
          where w :: Monad m => [Expression] -> m [Expression]
                w (e0@Store {value = Unresolved {unresolvedSymbol = "stg_MUT_ARR_PTRS_DIRTY_info"}}:Store {bytes = 1}:es) = do
                  new_es <- w es
                  pure $ e0 : new_es
                w (e0:e1:es) = do
                  new_e0 <- patchWritePtrArrayOp e0
                  new_e1_es <- w $ e1 : es
                  pure $ new_e0 : new_e1_es
                w es = patchWritePtrArrayOp es
        _ -> go
    _ -> go
  where
    go = gmapM patchWritePtrArrayOp t

maskUnknownCCallTargets ::
     (Monad m, Data a) => [AsteriusEntitySymbol] -> a -> m a
maskUnknownCCallTargets export_funcs = f
  where
    has_call_target sym =
      "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName sym) ||
      sym `M.member`
      functionMap (rtsAsteriusModule unsafeDefaultBuiltinsOptions) ||
      sym `elem` export_funcs
    f :: (Monad m, Data a) => a -> m a
    f t =
      let go = gmapM f t
       in case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
            Just HRefl ->
              case t of
                Call {..}
                  | not $ has_call_target target ->
                    pure $
                    emitErrorMessage
                      valueType
                      (entityName target <>
                       " failed: unimplemented stub function entered")
                  | target == "createIOThread" ->
                    case operands of
                      [cap, stack_size_w@Load {valueType = I32}, target_closure] ->
                        pure $
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
                      _ -> pure t
                  | otherwise -> pure t
                _ -> go
            _ -> go
