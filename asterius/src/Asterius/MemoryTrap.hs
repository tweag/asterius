{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Asterius.MemoryTrap
  ( addMemoryTrap
  , addMemoryTrapDeep
  ) where

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
          pure
            Call
              { target =
                  case (valueType, bytes) of
                    (I32, 1) -> "__asterius_Load_I8"
                    (I32, 2) -> "__asterius_Load_I16"
                    (I32, 4) -> "__asterius_Load_I32"
                    (I64, 8) -> "__asterius_Load_I64"
                    (F32, 4) -> "__asterius_Load_F32"
                    (F64, 8) -> "__asterius_Load_F64"
                    _ -> error $ "Unsupported instruction: " <> show t
              , operands = [new_i64_ptr, ConstI32 $ fromIntegral offset]
              , callReturnTypes = [valueType]
              }
        Store {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} -> do
          new_i64_ptr <- addMemoryTrapDeep i64_ptr
          new_value <- addMemoryTrapDeep value
          pure
            Call
              { target =
                  case (valueType, bytes) of
                    (I32, 1) -> "__asterius_Store_I8"
                    (I32, 2) -> "__asterius_Store_I16"
                    (I32, 4) -> "__asterius_Store_I32"
                    (I64, 8) -> "__asterius_Store_I64"
                    (F32, 4) -> "__asterius_Store_F32"
                    (F64, 8) -> "__asterius_Store_F64"
                    _ -> error $ "Unsupported instruction: " <> show t
              , operands =
                  [new_i64_ptr, ConstI32 $ fromIntegral offset, new_value]
              , callReturnTypes = []
              }
        _ -> go
    _ -> go
  where
    go = gmapM addMemoryTrapDeep t
