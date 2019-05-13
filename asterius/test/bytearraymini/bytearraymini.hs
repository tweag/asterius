{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O2 -fforce-recomp
  -Wall -ddump-to-file -ddump-simpl -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import GHC.Exts
import GHC.Types

{-
This test case tests the ability of the compiler to handle casts from I8/I16 to
I64 in a hand-written test case.

Expected output:
-1
255
-1
65535
-}

mainInt8 :: IO ()
mainInt8 = do
  r <-
    IO $ \s0 ->
      case newByteArray# 8# s0 of
        (# s1, mba #) ->
          case writeInt8Array# mba 0# 255# s1 of
            s2 ->
              case unsafeFreezeByteArray# mba s2 of
                (# s10, ba #) -> (# s10, I# (indexInt8Array# ba 0#) #)
  print r

mainInt16 :: IO ()
mainInt16 = do
  r <-
    IO $ \s0 ->
      case newByteArray# 16# s0 of
        (# s1, mba #) ->
          case writeInt16Array# mba 0# 65535# s1 of
            s2 ->
              case unsafeFreezeByteArray# mba s2 of
                (# s10, ba #) -> (# s10, I# (indexInt16Array# ba 0#) #)
  print r

mainWord8 :: IO ()
mainWord8 = do
  r <-
    IO $ \s0 ->
      case newByteArray# 8# s0 of
        (# s1, mba #) ->
          case writeWord8Array# mba 0# 255## s1 of
            s2 ->
              case unsafeFreezeByteArray# mba s2 of
                (# s10, ba #) -> (# s10, W# (indexWord8Array# ba 0#) #)
  print r

mainWord16 :: IO ()
mainWord16 = do
  r <-
    IO $ \s0 ->
      case newByteArray# 16# s0 of
        (# s1, mba #) ->
          case writeWord16Array# mba 0# 65535## s1 of
            s2 ->
              case unsafeFreezeByteArray# mba s2 of
                (# s10, ba #) -> (# s10, W# (indexWord16Array# ba 0#) #)
  print r

main :: IO ()
main = do
    mainInt8
    mainWord8
    mainInt16
    mainWord16
