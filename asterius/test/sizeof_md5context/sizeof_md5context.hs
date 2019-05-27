{-# LANGUAGE CPP #-}

#include "HsBaseConfig.h"
import Control.Exception (assert)
import GHC.Fingerprint
import Control.Monad

main :: IO ()
main = do
  assert (SIZEOF_STRUCT_MD5CONTEXT  == (88 :: Int)) (pure ())
  forM_ [1..10] $ \i -> print $ fingerprintString (replicate i ' ')

