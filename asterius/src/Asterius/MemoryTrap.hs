{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.MemoryTrap
  ( addMemoryTrap,
  )
where

import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Data.Data
  ( Data,
    gmapT,
  )
import Type.Reflection

addMemoryTrap :: AsteriusModule -> AsteriusModule
addMemoryTrap m =
  let new_function_map = SM.mapWithKey addMemoryTrapDeep (functionMap m)
   in m {functionMap = new_function_map}

addMemoryTrapDeep :: Data a => EntitySymbol -> a -> a
addMemoryTrapDeep sym = w
  where
    w :: Data a => a -> a
    w t = case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
      Just HRefl -> case t of
        Load {ptr = i32_ptr, ..} ->
          let new_i32_ptr = w i32_ptr
           in CallImport
                { target' =
                    "__asterius_load_"
                      <> load_fn_suffix valueType bytes signed,
                  operands =
                    [ Symbol {unresolvedSymbol = sym, symbolOffset = 0},
                      new_i32_ptr,
                      ConstI32 $ fromIntegral offset
                    ],
                  callImportReturnTypes = [valueType]
                }
        Store {ptr = i32_ptr, ..} ->
          let new_i32_ptr = w i32_ptr
              new_value = w value
           in CallImport
                { target' = "__asterius_store_" <> store_fn_suffix valueType bytes,
                  operands =
                    [ Symbol {unresolvedSymbol = sym, symbolOffset = 0},
                      new_i32_ptr,
                      ConstI32 $ fromIntegral offset,
                      new_value
                    ],
                  callImportReturnTypes = []
                }
        _ -> go
      _ -> go
      where
        go = gmapT w t
    load_fn_suffix I32 1 False = "I32_U8"
    load_fn_suffix I32 1 True = "I32_S8"
    load_fn_suffix I32 2 False = "I32_U16"
    load_fn_suffix I32 2 True = "I32_S16"
    load_fn_suffix I32 4 _ = "I32"
    load_fn_suffix I64 1 False = "I64_U8"
    load_fn_suffix I64 1 True = "I64_S8"
    load_fn_suffix I64 2 False = "I64_U16"
    load_fn_suffix I64 2 True = "I64_S16"
    load_fn_suffix I64 8 _ = "I64"
    load_fn_suffix F32 4 _ = "F32"
    load_fn_suffix F64 8 _ = "F64"
    load_fn_suffix _ _ _ = error "Asterius.MemoryTrap.addMemoryTrapDeep"
    store_fn_suffix I32 1 = "I8"
    store_fn_suffix I32 2 = "I16"
    store_fn_suffix I32 4 = "I32"
    store_fn_suffix I64 8 = "I64"
    store_fn_suffix F32 4 = "F32"
    store_fn_suffix F64 8 = "F64"
    store_fn_suffix _ _ = error "Asterius.MemoryTrap.addMemoryTrapDeep"
