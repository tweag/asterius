{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Asterius.MemoryTrap
  ( addMemoryTrap
  , addMemoryTrapDeep
  ) where

import Asterius.EDSL
import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapM)
import qualified Data.Map.Strict as M
import Data.Traversable
import Type.Reflection

addMemoryTrap :: Monad m => AsteriusModule -> m AsteriusModule
addMemoryTrap m = do
  new_function_map <-
    fmap M.fromList $
    for (M.toList $ functionMap m) $ \(func_sym, func) ->
      if "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName func_sym)
        then pure (func_sym, func)
        else (func_sym, ) <$> addMemoryTrapDeep func
  pure m {functionMap = new_function_map}

addMemoryTrapDeep :: (Monad m, Data a) => a -> m a
addMemoryTrapDeep t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Load {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} -> do
          new_i64_ptr <- addMemoryTrapDeep i64_ptr
          pure $
            castFrom valueType $
            CallImport
              { target' =
                  case (valueType, bytes) of
                    (I32, 1) -> "__asterius_load_I8"
                    (I32, 2) -> "__asterius_load_I16"
                    (I32, 4) -> "__asterius_load_I32"
                    (I64, 8) -> "__asterius_load_I64"
                    (F32, 4) -> "__asterius_load_F32"
                    (F64, 8) -> "__asterius_load_F64"
                    _ -> error $ "Unsupported instruction: " <> show t
              , operands =
                  [castTo I64 new_i64_ptr, ConstI32 $ fromIntegral offset]
              , callImportReturnTypes =
                  [ case valueType of
                      I64 -> F64
                      _ -> valueType
                  ]
              }
        Store {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} -> do
          new_i64_ptr <- addMemoryTrapDeep i64_ptr
          new_value <- addMemoryTrapDeep value
          pure
            CallImport
              { target' =
                  case (valueType, bytes) of
                    (I32, 1) -> "__asterius_store_I8"
                    (I32, 2) -> "__asterius_store_I16"
                    (I32, 4) -> "__asterius_store_I32"
                    (I64, 8) -> "__asterius_store_I64"
                    (F32, 4) -> "__asterius_store_F32"
                    (F64, 8) -> "__asterius_store_F64"
                    _ -> error $ "Unsupported instruction: " <> show t
              , operands =
                  [ castTo I64 new_i64_ptr
                  , ConstI32 $ fromIntegral offset
                  , castTo valueType new_value
                  ]
              , callImportReturnTypes = []
              }
        _ -> go
    _ -> go
  where
    go = gmapM addMemoryTrapDeep t

castFrom, castTo :: ValueType -> Expression -> Expression
castFrom I64 = truncSFloat64ToInt64
castFrom _ = id

castTo I64 = convertSInt64ToFloat64
castTo _ = id
