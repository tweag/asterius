{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Prim where

#if defined(ASTERIUS)

import GHC.Base

foreign import javascript "__asterius_jsffi.stdio.putChar(${1},${2})" js_putChar :: Int -> Char -> IO ()

#endif
