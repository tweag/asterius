import Data.Primitive.ByteArray
import Data.Word

main :: IO ()
main = do
  -- ---------------------------------------------------------------------
  -- MEMSET (setByteArray uses memset internally)
  -- ---------------------------------------------------------------------
  dst <- newPinnedByteArray 32
  unfreezeAndPrint "0: " dst
  -- Expected (little-endian):
  -- 0: [0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0]

  -- WORD32
  ---------
  setByteArray dst 0 1 (0xeeeeeeee :: Word32)
  setByteArray dst 4 2 (0xff1234ff :: Word32)
  unfreezeAndPrint "1: " dst
  -- Expected (little-endian):
  -- 1: [0xee, 0xee, 0xee, 0xee, 0xff, 0x34, 0x12, 0xff, 0xff, 0x34, 0x12, 0xff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0]
  readByteArray dst 0 >>= \w -> print (w == (0xeeeeeeee :: Word32))
  -- Expected (endianness-independent): True
  readByteArray dst 1 >>= \w -> print (w == (0xff1234ff :: Word32))
  -- Expected (endianness-independent): True

  -- WORD16
  -- ------
  setByteArray dst 2 2 (0x1122 :: Word16)
  unfreezeAndPrint "2: " dst
  -- Expected (little-endian):
  -- 2: [0xee, 0xee, 0x22, 0x11, 0x22, 0x11, 0x12, 0xff, 0xff, 0x34, 0x12, 0xff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0]
  readByteArray dst 1 >>= \w -> print (w == (0x1122 :: Word16))
  -- Expected (endianness-independent): True

  -- CHAR
  -- ----
  setByteArray dst 24 2 ('a' :: Char)
  unfreezeAndPrint "3: " dst
  -- Expected (little-endian):
  -- 3: [0xee, 0xee, 0x22, 0x11, 0x22, 0x11, 0x12, 0xff, 0xff, 0x34, 0x12, 0xff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x61, 0x0, 0x0, 0x0, 0x61, 0x0, 0x0, 0x0]
  readByteArray dst 6 >>= \c -> print (c == 'a')
  -- Expected (endianness-independent): True
  readByteArray dst 7 >>= \c -> print (c == 'a')
  -- Expected (endianness-independent): True

  -- WORD8
  -- -----
  setByteArray dst 30 1 (0xec :: Word8)
  unfreezeAndPrint "4: " dst
  -- Expected (endianness-independent):
  -- 4: [0xee, 0xee, 0x22, 0x11, 0x22, 0x11, 0x12, 0xff, 0xff, 0x34, 0x12, 0xff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x61, 0x0, 0x0, 0x0, 0x61, 0x0, 0xec, 0x0]
  readByteArray dst 30 >>= \w -> print (w == (0xec :: Word8))
  -- Expected (endianness-independent): True

  -- FLOAT
  -- -----
  setByteArray dst 16 1 (-1 :: Float)
  unfreezeAndPrint "5: " dst
  -- Expected (little-endian):
  -- 5: [0xee, 0xee, 0x22, 0x11, 0x22, 0x11, 0x12, 0xff, 0xff, 0x34, 0x12, 0xff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x80, 0xbf, 0x0, 0x0, 0x0, 0x0, 0x61, 0x0, 0x0, 0x0, 0x61, 0x0, 0xec, 0x0]
  readByteArray dst 4 >>= \f -> print (f == (-1 :: Float))
  -- Expected (endianness-independent): True

  -- DOUBLE
  -- ------
  setByteArray dst 8 1 (100 :: Double)
  unfreezeAndPrint "6: " dst
  -- Expected (little-endian):
  -- TODO
  readByteArray dst 1 >>= \d -> print (d == (100 :: Double))
  -- Expected (endianness-independent): True

  -- WORD64
  -- ------
  -- setByteArray dst 24 1 (0xcccccccccccccccc :: Word64)
  -- unfreezeAndPrint "5: " dst
  -- -- Expected (little-endian):
  -- -- TODO
  -- readByteArray dst 3 >>= \w -> print (w == (0xcccccccccccccccc :: Word64))
  -- -- Expected (endianness-independent): True
  -- -- THROWS THIS AT ME:
  -- --
  -- -- ==>> TypeError: Cannot convert 0 to a BigInt
  -- -- ==>>     at BigUint64Array.fill (<anonymous>)
  -- -- ==>>     at Memory.memset (file:///home/skull/tweag/asterius/asterius/test/primitive/rts.memory.mjs:311:9)
  -- -- ==>>     at wasm-function[2780]:0x950e1
  -- -- ==>>     at wasm-function[37]:0x3466
  -- -- ==>>     at wasm-function[3056]:0x9ff0c
  -- -- ==>>     at wasm-function[3057]:0x9ff39
  -- -- ==>>     at Scheduler.tick (file:///home/skull/tweag/asterius/asterius/test/primitive/rts.scheduler.mjs:347:22)
  -- -- ==>>     at Immediate.<anonymous> (file:///home/skull/tweag/asterius/asterius/test/primitive/rts.scheduler.mjs:382:29)
  -- -- ==>>     at processImmediate (internal/timers.js:456:21)

  -- WORD (should be the same as with word64)
  -- ----------------------------------------
  -- TODO

  where
    unfreezeAndPrint s ba =
      unsafeFreezeByteArray ba >>= \a -> putStr s >> print a


