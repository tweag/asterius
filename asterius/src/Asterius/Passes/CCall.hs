{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.CCall
  ( handleCCall,
  )
where

import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

handleCCall ::
  M.Map BS.ByteString (BS.ByteString, FunctionType) ->
  Expression ->
  Expression
handleCCall libc_func_info e@Call {callHint = Just (_, res_hints), ..}
  | Just (in_name, FunctionType {..}) <-
      entityName target `M.lookup` libc_func_info =
    let res_trans = case (returnTypes, res_hints, callReturnTypes) of
          ([_], [_], []) -> Drop
          _ -> id
     in res_trans
          e
            { target = mkEntitySymbol in_name,
              operands = operands,
              callReturnTypes = returnTypes,
              callHint = Nothing
            }
handleCCall _ e = e
