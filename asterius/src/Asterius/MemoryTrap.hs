{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.MemoryTrap
  ( addMemoryTrap
  ) where

import Asterius.Types
import Data.Data (Data, gmapT)
import qualified Data.Map.Strict as M
import Type.Reflection

addMemoryTrap :: AsteriusModule -> AsteriusModule
addMemoryTrap m =
  let new_function_map = M.mapWithKey addMemoryTrapDeep (functionMap m)
   in m {functionMap = new_function_map}

addMemoryTrapDeep :: Data a => AsteriusEntitySymbol -> a -> a
addMemoryTrapDeep func_sym = w
  where
    w :: Data b => b -> b
    w t =
      case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
        Just HRefl ->
          case t of
            Load {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} ->
              let new_i64_ptr = w i64_ptr
               in CallImport
                    { target' =
                        "__asterius_load_" <> ty_name func_sym t valueType bytes
                    , operands = [new_i64_ptr, ConstI32 $ fromIntegral offset]
                    , callImportReturnTypes = [valueType]
                    }
            Store {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} ->
              let new_i64_ptr = w i64_ptr
                  new_value = w value
               in CallImport
                    { target' =
                        "__asterius_store_" <>
                        ty_name func_sym t valueType bytes
                    , operands =
                        [new_i64_ptr, ConstI32 $ fromIntegral offset, new_value]
                    , callImportReturnTypes = []
                    }
            _ -> go
        _ -> go
      where
        go = gmapT w t
    ty_name _ _ I32 1 = "I8"
    ty_name _ _ I32 2 = "I16"
    ty_name _ _ I32 4 = "I32"
    ty_name _ _ I64 8 = "I64"
    ty_name _ _ F32 4 = "F32"
    ty_name _ _ F64 8 = "F64"
    ty_name sym e vt b =
      error $
      "Asterius.MemoryTrap.addMemoryTrapDeep: " <> show sym <> " " <> show vt <>
      " " <>
      show b <>
      " " <>
      show e
