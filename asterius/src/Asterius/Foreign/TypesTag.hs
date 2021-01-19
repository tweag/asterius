module Asterius.Foreign.TypesTag
  ( ffiValueTypesTag,
  )
where

import Asterius.Types
import Data.Bits
import Data.Foldable

ffiValueTypeTag :: FFIValueType -> Word
ffiValueTypeTag = succ . fromIntegral . fromEnum

ffiValueTypesTag :: [FFIValueType] -> Word
ffiValueTypesTag =
  foldr' (\vt acc -> (acc `shiftL` 5) .|. ffiValueTypeTag vt) 0
