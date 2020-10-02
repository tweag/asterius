{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.CCall
  ( handleCCall,
  )
where

import Asterius.EDSL
import Asterius.Internals.MagicNumber
import Asterius.TypeInfer
import Asterius.Types
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

handleCCall ::
  M.Map BS.ByteString (BS.ByteString, FunctionType) ->
  Expression ->
  Expression
handleCCall libc_func_info e@Call {callHint = Just (args_hints, res_hints), ..}
  | Just (in_name, FunctionType {..}) <-
      entityName target `M.lookup` libc_func_info =
    let res_trans = case (returnTypes, res_hints, callReturnTypes) of
          ([_], [_], []) -> Drop
          ([I32], [NoHint], [I64]) -> extendUInt32
          ([I32], [AddrHint], [I64]) ->
            (constI64 ((fromIntegral dataTag) `shiftL` 32) `orInt64`)
              . extendUInt32
          ([I32], [SignedHint], [I64]) -> extendSInt32
          _ -> id
     in res_trans
          e
            { target = mkEntitySymbol in_name,
              operands =
                [ case (t1, h, t0) of
                    (I32, _, [I64]) -> wrapInt64 x
                    _ -> x
                  | (x, t1, h) <- zip3 operands paramTypes args_hints,
                    let t0 = infer x
                ],
              callReturnTypes = returnTypes,
              callHint = Nothing
            }
handleCCall _ e = e
