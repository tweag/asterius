{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Binary
import Distribution.Simple
import Distribution.Simple.Program

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = map simpleProgram ["mkdir", "cp", "node", "sed", "sh"]
      , postConf =
          \args flags pkg_descr lbi -> do
            encodeFile ".lbi.buildinfo" lbi
            postConf simpleUserHooks args flags pkg_descr lbi
      }
