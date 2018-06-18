{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.MemoryTrap
  ( addMemoryTrap
  ) where

import Asterius.Builtins
import Asterius.Types
import Data.Data (Data, gmapT)
import qualified Data.Vector as V
import Type.Reflection

addMemoryTrap :: Data a => a -> a
addMemoryTrap t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Load {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} ->
          Block
            { name = ""
            , bodys =
                V.fromList $
                [ CallImport
                  { target' = "__asterius_load_i64"
                  , operands =
                      cutI64 i64_ptr <>
                      cutI64
                        Load
                          { signed = False
                          , bytes = 8
                          , offset = 0
                          , align = 0
                          , valueType = I64
                          , ptr =
                              Unary {unaryOp = WrapInt64, operand0 = i64_ptr}
                          }
                  , valueType = None
                  }
                | valueType == I64
                ] <>
                [ Call
                    { target = "__asterius_memory_trap"
                    , operands = [i64_ptr]
                    , valueType = None
                    }
                , t
                ]
            , valueType = valueType
            }
        Store {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} ->
          Block
            { name = ""
            , bodys =
                V.fromList $
                [ CallImport
                  { target' = "__asterius_store_i64"
                  , operands = cutI64 i64_ptr <> cutI64 value
                  , valueType = None
                  }
                | valueType == I64
                ] <>
                [ Call
                    { target = "__asterius_memory_trap"
                    , operands = [i64_ptr]
                    , valueType = None
                    }
                , t
                ]
            , valueType = None
            }
        _ -> go
    _ -> go
  where
    go = gmapT addMemoryTrap t
