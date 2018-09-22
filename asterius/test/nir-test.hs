{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

import Asterius.Marshal
import Asterius.Types
import Bindings.Binaryen.Raw
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Foreign
import qualified Language.WebAssembly.WireFormat as Wasm

main :: IO ()
main
  -- c_BinaryenSetAPITracing 1
 = do
  c_BinaryenSetOptimizeLevel 0
  c_BinaryenSetShrinkLevel 0
  m <-
    withPool $ \pool ->
      marshalModule pool $
      Module
        { functionTypeMap = [("func_type", FunctionType I32 [])]
        , functionMap' =
            [ ( "func"
              , Function "func_type" [I32, I64, I64, I32] $
                Block
                  mempty
                  [Block mempty [Block mempty [GetLocal 3 I32] I32] I32]
                  I32)
            ]
        , functionImports = []
        , functionExports = []
        , functionTable = FunctionTable []
        , memory = Memory 1 1 mempty mempty
        }
  c_BinaryenModuleValidate m >>= print
  buf <- LBS.fromStrict <$> serializeModule m
  let r = runGet Wasm.getModule buf
  print r
