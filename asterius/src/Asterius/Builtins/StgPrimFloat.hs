{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.StgPrimFloat
  ( stgPrimFloatCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

stgPrimFloatCBits :: AsteriusModule
stgPrimFloatCBits = wordEncodeDouble <> wordEncodeFloat

wordEncodeDouble, wordEncodeFloat :: AsteriusModule
wordEncodeDouble = runEDSL "__word_encodeDouble" $ do
  setReturnTypes [F64]
  [j, e] <- params [I64, I64]
  r <- local F64 $ convertUInt64ToFloat64 j
  r' <- call' "scalbn" [r, truncExponent e] F64
  emit
    If
      { condition = r `neFloat64` ConstF64 0.0,
        ifTrue = r',
        ifFalse = Just r
      }
wordEncodeFloat = runEDSL "__word_encodeFloat" $ do
  setReturnTypes [F32]
  [j, e] <- params [I64, I64]
  r <- call' "__word_encodeDouble" [j, e] F64
  emit $ demoteFloat64 r

truncExponent :: Expression -> Expression
truncExponent e =
  wrapInt64
    If
      { condition =
          e `gtSInt64` ConstI64 2147483647,
        ifTrue = ConstI64 2147483647,
        ifFalse =
          Just
            If
              { condition =
                  e
                    `ltSInt64` ConstI64
                      (-2147483648),
                ifTrue = ConstI64 (-2147483648),
                ifFalse = Just e
              }
      }
