{-# LANGUAGE CPP #-}

#include "HsBaseConfig.h"
import Control.Exception (assert)

main :: IO ()
main = assert (SIZEOF_STRUCT_MD5CONTEXT  == (88 :: Int)) (pure ())

