{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.MemoryTrap
  ( addMemoryTrap
  , addMemoryTrapDeep
  ) where

import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Data (Data, gmapT)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Type.Reflection

addMemoryTrap :: AsteriusModule -> AsteriusModule
addMemoryTrap m =
  m
    { functionMap =
        HM.mapWithKey
          (\func_sym func ->
             if "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName func_sym)
               then func
               else addMemoryTrapDeep func)
          (functionMap m)
    }

addMemoryTrapDeep :: Data a => a -> a
addMemoryTrapDeep t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Load {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} ->
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
            , operands = [addMemoryTrapDeep i64_ptr]
            , valueType = valueType
            }
        Store {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} ->
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
            , operands = [addMemoryTrapDeep i64_ptr, addMemoryTrapDeep value]
            , valueType = None
            }
        Host {hostOp = CurrentMemory} ->
          CallImport
            { target' = "__asterius_current_memory"
            , operands = [t]
            , valueType = I32
            }
        Host {hostOp = GrowMemory, ..} ->
          CallImport
            { target' = "__asterius_grow_memory"
            , operands = [t, V.head operands]
            , valueType = I32
            }
        _ -> go
    _ -> go
  where
    go = gmapT addMemoryTrapDeep t
