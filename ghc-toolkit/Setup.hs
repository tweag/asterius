{-# OPTIONS_GHC -Wall #-}

import Data.Binary
import Distribution.Simple

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { postConf =
          \args flags pkg_descr lbi -> do
            encodeFile ".ghc-toolkit.buildinfo" lbi
            postConf simpleUserHooks args flags pkg_descr lbi
      }
