{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Bindings.Binaryen.Raw
import qualified Data.ByteString as BS
import Data.Functor
import Foreign.ForeignPtr
import GHC.Exts
import Language.WebAssembly.NIR

f :: Int -> Expression
f n = Block "blockout" [Block "blockdef" [w n] None, GetLocal 1 I32] I32
  where
    w 0 =
      Switch
        (fromList ["block" <> fromString (show i) | i <- [0 .. n - 1]])
        "blockdef"
        (GetLocal 0 I32)
        Null
    w x =
      Block
        ("block" <> fromString (show (x - 1)))
        [ w (x - 1)
        , SetLocal 1 (ConstI32 (fromIntegral (x - 1)))
        , Break "blockdef" Null Null
        ]
        None

main :: IO ()
main = do
  m <- c_BinaryenModuleCreate
  void $
    marshalFunction m $
    Function "func" (FunctionType "func_type" I32 [I32]) [I32] $ f 1000
  fptr <- mallocForeignPtrBytes 100000000
  (s, bs) <-
    withForeignPtr fptr $ \ptr -> do
      s <- c_BinaryenModuleWrite m ptr 100000000
      bs <- BS.packCStringLen (ptr, fromIntegral s)
      finalizeForeignPtr fptr
      pure (s, bs)
  c_BinaryenModuleDispose m
  print s
  BS.writeFile "nir.wasm_o" bs
