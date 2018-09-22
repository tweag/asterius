{-# OPTIONS_GHC -Wno-overflowed-literals #-}

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Language.WebAssembly.WireFormat
import Language.WebAssembly.WireFormat.Orphans
import Test.QuickCheck
import Test.QuickCheck.Gen

testCodecGen ::
     (Eq a, Show a) => Gen a -> (a -> [a]) -> Get a -> (a -> Put) -> Property
testCodecGen gen s g p =
  forAllShrink gen s $ \x ->
    let buf = runPut $ p x
     in case runGetOrFail g buf of
          Right (rest, _, r)
            | r /= x || not (LBS.null rest) ->
              counterexample
                ("testCodeGen: failed to parse.\nBefore: " <> show x <>
                 "\nBuffer: " <>
                 show buf <>
                 "\nAfter: " <>
                 show r <>
                 "\nResidule: " <>
                 show rest)
                False
            | otherwise -> property True
          _ ->
            counterexample
              ("testCodecGen: failed to parse.\nBefore: " <> show x <>
               "\nBuffer: " <>
               show buf)
              False

testCodecFile ::
     (Eq a, Show a)
  => LBS.ByteString
  -> (a -> [a])
  -> Get a
  -> (a -> Put)
  -> Property
testCodecFile buf s g p =
  case runGetOrFail g buf of
    Right (rest, _, x)
      | LBS.null rest -> testCodecGen (pure x) s g p
      | otherwise ->
        counterexample
          ("testCodecFile: failed to parse.\nBuffer: " <> show buf <>
           "\nResult: " <>
           show x <>
           "\nResidule: " <>
           show rest)
          False
    _ ->
      counterexample
        ("testCodecFile: failed to parse.\nBuffer: " <> show buf)
        False

testCodecModule :: FilePath -> IO Property
testCodecModule p = do
  buf <- LBS.readFile p
  pure $ testCodecFile buf genericShrink getModule putModule

testLEB128Static :: Property
testLEB128Static =
  conjoin
    [ testCodecGen (pure 0) (const []) getVU32 putVU32
    , testCodecGen (pure maxBound) (const []) getVU32 putVU32
    , testCodecGen (pure minBound) (const []) getVS32 putVS32
    , testCodecGen (pure (-1)) (const []) getVS32 putVS32
    , testCodecGen (pure 0) (const []) getVS32 putVS32
    , testCodecGen (pure maxBound) (const []) getVS32 putVS32
    , testCodecGen (pure minBound) (const []) getVS64 putVS64
    , testCodecGen (pure (-1)) (const []) getVS64 putVS64
    , testCodecGen (pure 0) (const []) getVS64 putVS64
    , testCodecGen (pure maxBound) (const []) getVS64 putVS64
    , testCodecGen (pure 0xFFFFFFFFFFFFFFF8) (const []) getVS64 putVS64
    ]

testLEB128Dynamic :: Property
testLEB128Dynamic =
  conjoin
    [ testCodecGen chooseAny (const []) getVU32 putVU32
    , testCodecGen chooseAny (const []) getVS32 putVS32
    , testCodecGen chooseAny (const []) getVS64 putVS64
    , testCodecGen chooseAny (const []) getF32 putF32
    , testCodecGen chooseAny (const []) getF64 putF64
    ]

main :: IO ()
main = do
  quickCheck testLEB128Static
  for_
    [ "test/array.wasm"
    , "test/clang-fib.wasm"
    , "test/fib.wasm"
    , "test/jsffi.wasm"
    , "test/rtsapi.wasm"
    , "test/stableptr.wasm"
    ] $
    testCodecModule >=> quickCheck
  r <-
    quickCheckResult $
    withMaxSuccess 576460752303423488 $
    conjoin
      [ testLEB128Dynamic
      , testCodecGen genModule genericShrink getModule putModule
      ]
  writeFile "test/wasm-toolkit-codec.txt" $ show r
