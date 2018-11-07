{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-simpl -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import GHC.Exts
import GHC.Types

unI# :: Int -> Int#
unI# (I# x) = x

fill ::
     MutableByteArray# RealWorld
  -> (Int -> Int)
  -> State# RealWorld
  -> State# RealWorld
fill mba f s0 =
  case getSizeofMutableByteArray# mba s0 of
    (# s1, len #) -> w len 0# s1
  where
    w :: Int# -> Int# -> State# RealWorld -> State# RealWorld
    w len i s =
      case i ==# len of
        0# -> w len (i +# 1#) (writeInt8Array# mba i (unI# (f (I# i))) s)
        _ -> s

load :: ByteArray# -> [Int]
load ba =
  let len = sizeofByteArray# ba
      w i acc =
        case i ==# len of
          0# -> w (i +# 1#) (I# (indexInt8Array# ba i) : acc)
          _ -> reverse acc
   in w 0# []

m :: IO [Int]
m =
  IO
    (\s0 ->
       case newPinnedByteArray# 8# s0 of
         (# s1, mba #) ->
           case fill mba (+ 1) s1 of
             s2 ->
               case unsafeFreezeByteArray# mba s2 of
                 (# s3, ba #) -> (# s3, load ba #))

main :: IO ()
main = m >>= print
