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
  r <- local F64 $ Unary ConvertUInt64ToFloat64 j
  r' <- call' "scalbn" [r, truncExponent e] F64
  emit If
    { condition = Binary NeFloat64 r (ConstF64 0.0),
      ifTrue = r',
      ifFalse = Just r
    }
wordEncodeFloat = runEDSL "__word_encodeFloat" $ do
  setReturnTypes [F32]
  [j, e] <- params [I64, I64]
  r <- call' "__word_encodeDouble" [j, e] F64
  emit $ Unary DemoteFloat64 r

truncExponent :: Expression -> Expression
truncExponent e = Unary
  { unaryOp = WrapInt64,
    operand0 = If
      { condition = Binary
          { binaryOp = GtSInt64,
            operand0 = e,
            operand1 = ConstI64 2147483647
          },
        ifTrue = ConstI64 2147483647,
        ifFalse = Just If
          { condition = Binary
              { binaryOp = LtSInt64,
                operand0 = e,
                operand1 =
                  ConstI64
                    (-2147483648)
              },
            ifTrue = ConstI64 (-2147483648),
            ifFalse = Just e
          }
      }
  }
