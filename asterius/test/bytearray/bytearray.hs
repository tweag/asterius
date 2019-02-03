{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-simpl -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Control.Concurrent
import Control.Monad
import Data.Foldable
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

m :: Int -> IO (Int, Int)
m (I# len) =
  IO
    (\s0 ->
       case newPinnedByteArray# len s0 of
         (# s1, mba #) ->
           case fill mba (+ 1) s1 of
             s2 ->
               case unsafeFreezeByteArray# mba s2 of
                 (# s3, ba #) ->
                   (# s3
                    , ( I# (indexInt8Array# ba 0#)
                      , I# (indexInt8Array# ba (len -# 1#)))#))

src :: [Int]
src = w 0 []
  where
    w 20000 acc = acc
    w i acc = w (succ i) (i : acc)

main :: IO ()
main = do
  for_ [63, 511, 8191, 65535] $ m >=> print >=> const yield
  print $ foldl' (+) (0 :: Int) src
