{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSArray
  ( JSArray (..),
    fromJSArray,
    toJSArray,
  )
where

import Asterius.Magic
import Asterius.Types.JSVal
import GHC.Base
import GHC.Enum

newtype JSArray
  = JSArray JSVal

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

foreign import javascript unsafe "$1.length" js_arr_len :: JSArray -> Int

foreign import javascript unsafe "$1[$2]" js_arr_idx :: JSArray -> Int -> JSVal

foreign import javascript unsafe "[]" js_arr_new :: IO JSArray

foreign import javascript unsafe "$1.push($2)" js_arr_push :: JSArray -> JSVal -> IO ()
