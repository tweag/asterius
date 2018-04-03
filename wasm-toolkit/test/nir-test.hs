{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Bindings.Binaryen.Raw
import qualified Data.ByteString.Short as SBS
import Data.Functor
import Data.String
import qualified Data.Vector as V
import Language.WebAssembly.NIR

maxn :: Int
maxn = 2

blockTag :: Int -> SBS.ShortByteString
blockTag = ("block" <>) . fromString . show

f :: Int -> Expression
f 0 =
  Block
    (blockTag 0)
    [ Switch
        (V.fromList [blockTag i | i <- [0 .. maxn]])
        "block_out"
        (GetLocal 0 I32)
        (ConstI32 233)
    ]
    Auto
f n =
  Block (blockTag n) [f $ n - 1, Return $ ConstI32 $ fromIntegral $ n - 1] Auto

main :: IO ()
main = do
  m <- c_BinaryenModuleCreate
  void $
    marshalFunction m $
    Function "func" (FunctionType "func_type" I32 [I32]) [] $
    Block "block_out" [f maxn, Return $ ConstI32 $ fromIntegral maxn] Auto
  c_BinaryenModuleValidate m >>= print
  c_BinaryenModuleDispose m
