{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O2 -fforce-recomp
  -Wall -ddump-to-file -ddump-simpl -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import GHC.Exts
import GHC.Types

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
main :: IO ()
main = do
    mainInt8
    mainWord8
