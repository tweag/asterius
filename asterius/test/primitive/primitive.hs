{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

-- NEW STUFF
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
----

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Debug.Trace (trace)
import GHC.Generics
import System.Mem

-- NEW STUFF
import Foreign.C.Types
import GHC.Exts
import Data.Primitive.MachDeps (Word64_#, Int64_#)
----

primitive :: Int -> Int
primitive n = go 0 1 0
  where
    go !acc0 acc1 i
      | i == n = acc0
      | otherwise = go acc1 (acc0 + acc1) (i + 1)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

facts :: [Int]
facts = scanl (*) 1 [1 ..]

factMap :: Int -> IM.IntMap Int
factMap n = IM.fromList $ take n $ zip [0 ..] facts

sumFacts :: Int -> Int
sumFacts n = fst $ flip execState (0, 0) $ fix $ \w -> do
  (tot, i) <- get
  put (tot + facts !! i, i + 1)
  when (i < n) w

data BinTree
  = Tip
  | Branch !BinTree !BinTree
  deriving (Eq, Ord, Generic)

instance NFData BinTree

genBinTree :: Int -> BinTree
genBinTree 0 = Tip
genBinTree n = Branch t t where t = genBinTree (n - 1)

sizeofBinTree :: BinTree -> Int
sizeofBinTree Tip = 1
sizeofBinTree (Branch x y) = 1 + sizeofBinTree x + sizeofBinTree y

-------------------------------------------------------------------------------
foreign import ccall unsafe "hsprimitive_memset_Word8"
  setWord8Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Word# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word16"
  setWord16Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Word# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word32"
  setWord32Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Word# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word64"
  setWord64Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Word64_# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word"
  setWordArray# :: MutableByteArray# s -> CPtrdiff -> CSize -> Word# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word8"
  setInt8Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Int# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word16"
  setInt16Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Int# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word32"
  setInt32Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Int# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word64"
  setInt64Array# :: MutableByteArray# s -> CPtrdiff -> CSize -> Int64_# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word"
  setIntArray# :: MutableByteArray# s -> CPtrdiff -> CSize -> Int# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Ptr"
  setAddrArray# :: MutableByteArray# s -> CPtrdiff -> CSize -> Addr# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Ptr"
  setStablePtrArray# :: MutableByteArray# s -> CPtrdiff -> CSize -> StablePtr# a -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Float"
  setFloatArray# :: MutableByteArray# s -> CPtrdiff -> CSize -> Float# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Double"
  setDoubleArray# :: MutableByteArray# s -> CPtrdiff -> CSize -> Double# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Char"
  setWideCharArray# :: MutableByteArray# s -> CPtrdiff -> CSize -> Char# -> IO ()

foreign import ccall unsafe "hsprimitive_memset_Word8"
  setWord8OffAddr# :: Addr# -> CPtrdiff -> CSize -> Word# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word16"
  setWord16OffAddr# :: Addr# -> CPtrdiff -> CSize -> Word# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word32"
  setWord32OffAddr# :: Addr# -> CPtrdiff -> CSize -> Word# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word64"
  setWord64OffAddr# :: Addr# -> CPtrdiff -> CSize -> Word64_# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word"
  setWordOffAddr# :: Addr# -> CPtrdiff -> CSize -> Word# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word8"
  setInt8OffAddr# :: Addr# -> CPtrdiff -> CSize -> Int# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word16"
  setInt16OffAddr# :: Addr# -> CPtrdiff -> CSize -> Int# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word32"
  setInt32OffAddr# :: Addr# -> CPtrdiff -> CSize -> Int# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word64"
  setInt64OffAddr# :: Addr# -> CPtrdiff -> CSize -> Int64_# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Word"
  setIntOffAddr# :: Addr# -> CPtrdiff -> CSize -> Int# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Ptr"
  setAddrOffAddr# :: Addr# -> CPtrdiff -> CSize -> Addr# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Ptr"
  setStablePtrOffAddr# :: Addr# -> CPtrdiff -> CSize -> StablePtr# a -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Float"
  setFloatOffAddr# :: Addr# -> CPtrdiff -> CSize -> Float# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Double"
  setDoubleOffAddr# :: Addr# -> CPtrdiff -> CSize -> Double# -> IO ()
foreign import ccall unsafe "hsprimitive_memset_Char"
  setWideCharOffAddr# :: Addr# -> CPtrdiff -> CSize -> Char# -> IO ()

-------------------------------------------------------------------------------

foreign import ccall safe "print_i64" print_i64 :: Int -> IO ()

foreign import ccall unsafe "assert_eq_i64" assert_eq_i64 :: Int -> Int -> IO ()

foreign import ccall unsafe "print_f64" print_f64 :: Double -> IO ()

readDouble :: IO ()
readDouble =
  let r = read "1.2"
   in if (r :: Double) == 1.2 then pure () else fail $ "read value: " <> show r

main :: IO ()
main = do
  performGC
  putStrLn $ trace "trace message" ""
  readDouble
  -- Test that assert_eq works
  assert_eq_i64 10 10
  print_i64 $ primitive 10
  assert_eq_i64 (primitive 10) 55
  print_i64 $ fact 5
  assert_eq_i64 (fact 5) 120
  print_f64 $ cos 0.5
  print_f64 $ 2 ** 3
  let sizeof3Tree = sizeofBinTree $ force $ genBinTree 3
  print_i64 $ sizeof3Tree
  -- 2^4 - 1
  assert_eq_i64 sizeof3Tree 15
  let sizeof5Tree = sizeofBinTree $ force $ genBinTree 5
  print_i64 $ sizeof5Tree
  -- 2^6 - 1
  assert_eq_i64 sizeof5Tree 63
  print_i64 $ facts !! 5
  assert_eq_i64 (facts !! 5) 120
  let factmapAt5 = factMap 10 IM.! 5
  print_i64 $ factmapAt5
  assert_eq_i64 factmapAt5 120
  -- 0! + 1! + 2! + 3! + 4! + 5!
  print_i64 $ sumFacts 5
  assert_eq_i64 (sumFacts 5) (154)
  performGC
