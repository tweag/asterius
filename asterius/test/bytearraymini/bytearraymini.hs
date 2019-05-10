{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC
  -Wall -ddump-to-file -ddump-simpl -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import GHC.Exts
import GHC.Types

main :: IO ()
main = do
  r <-
    IO $ \s0 ->
      case newByteArray# 8# s0 of
        (# s1, mba #) ->
          case writeInt8Array# mba 0# 255# s1 of
            s2 ->
              case writeInt8Array# mba 1# 255# s2 of
                s3 ->
                  case writeInt8Array# mba 2# 255# s3 of
                    s4 ->
                      case writeInt8Array# mba 3# 255# s4 of
                        s5 ->
                          case writeInt8Array# mba 4# 255# s5 of
                            s6 ->
                              case writeInt8Array# mba 5# 255# s6 of
                                s7 ->
                                  case writeInt8Array# mba 6# 255# s7 of
                                    s8 ->
                                      case writeInt8Array# mba 7# 255# s8 of
                                        s9 ->
                                          case unsafeFreezeByteArray# mba s9 of
                                            (# s10, ba #) ->
                                              (# s10
                                               , I# (indexInt8Array# ba 0#)#)
  print r
