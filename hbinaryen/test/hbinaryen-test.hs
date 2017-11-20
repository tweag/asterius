{-# LANGUAGE OverloadedLists #-}

import Bindings.Binaryen.Raw
import qualified Data.Vector.Storable as SV
import Foreign
import Foreign.C

main :: IO ()
main = do
  c_BinaryenSetAPITracing 1
  m <- c_BinaryenModuleCreate
  ft <-
    withCString "add_func_type" $ \p0 ->
      SV.unsafeWith [c_BinaryenTypeInt32, c_BinaryenTypeInt32] $ \p1 ->
        c_BinaryenAddFunctionType m p0 c_BinaryenTypeInt32 p1 2
  x <- c_BinaryenGetLocal m 0 c_BinaryenTypeInt32
  y <- c_BinaryenGetLocal m 1 c_BinaryenTypeInt32
  tot <- c_BinaryenBinary m c_BinaryenAddInt32 x y
  _ <-
    withCString "add_func" $ \p0 -> c_BinaryenAddFunction m p0 ft nullPtr 0 tot
  c_BinaryenModulePrint m
  c_BinaryenModuleDispose m
