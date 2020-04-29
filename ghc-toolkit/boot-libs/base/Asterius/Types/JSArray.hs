{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSArray
  ( JSArray (..),
    fromJSArray,
    toJSArray,
  )
where

import Asterius.Magic
import Asterius.Types.JSString ()
import Asterius.Types.JSVal
import GHC.Base
import GHC.Enum
import GHC.Show

newtype JSArray
  = JSArray JSVal
  deriving (Show)

{-# INLINEABLE fromJSArray #-}
fromJSArray :: JSArray -> [JSVal]
fromJSArray arr = w 0
  where
    len = js_arr_len arr
    w i
      | i < len = js_arr_idx arr i : w (succ i)
      | otherwise = []

{-# INLINEABLE toJSArray #-}
toJSArray :: [JSVal] -> JSArray
toJSArray l = accursedUnutterablePerformIO $ do
  arr <- js_arr_new
  let w (v : vs) = js_arr_push arr v *> w vs
      w [] = pure ()
   in w l
  pure arr

instance Eq JSArray where
  {-# INLINE (==) #-}
  arr0@(JSArray v0) == arr1@(JSArray v1)
    | v0 == v1 = True
    | otherwise = fromJSArray arr0 == fromJSArray arr1

instance Ord JSArray where
  {-# INLINE (<=) #-}
  arr0@(JSArray v0) <= arr1@(JSArray v1)
    | v0 == v1 = True
    | otherwise = fromJSArray arr0 <= fromJSArray arr1

foreign import javascript unsafe "$1.length" js_arr_len :: JSArray -> Int

foreign import javascript unsafe "$1[$2]" js_arr_idx :: JSArray -> Int -> JSVal

foreign import javascript unsafe "[]" js_arr_new :: IO JSArray

foreign import javascript unsafe "$1.push($2)" js_arr_push :: JSArray -> JSVal -> IO ()
