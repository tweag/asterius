{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Bindings.Binaryen.Raw
import Data.Functor
import Language.WebAssembly.NIR

main :: IO ()
main = do
  m <- c_BinaryenModuleCreate
  void $
    marshalFunction m $
    Function "func" (FunctionType "func_type" I32 [I32]) [] $
    Binary AddInt32 (GetLocal 0 I32) (GetLocal 0 I32)
  c_BinaryenModulePrint m
  c_BinaryenModuleValidate m >>= print
  c_BinaryenModuleDispose m
