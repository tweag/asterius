{-# LANGUAGE RecordWildCards #-}

module Asterius.Foreign.TypesTag
  ( ffiValueTypesTag,
  )
where

import Asterius.Types
import Data.Bits
import Data.Foldable

ffiValueTypeTag :: FFIValueType -> Int
ffiValueTypeTag FFIValueType {..} = case ffiValueTypeRep of
  FFIJSValRep -> 1
  FFIFloatRep -> 3
  FFIDoubleRep -> 4
  _ -> 2

ffiValueTypesTag :: [FFIValueType] -> Int
ffiValueTypesTag =
  foldr' (\vt acc -> (acc `shiftL` 3) .|. ffiValueTypeTag vt) 0
