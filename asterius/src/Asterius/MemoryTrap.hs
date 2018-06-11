{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.MemoryTrap
  ( addMemoryTrap
  ) where

import Asterius.Types
import Data.Data (Data, gmapT)
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
