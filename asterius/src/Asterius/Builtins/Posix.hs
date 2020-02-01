{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Posix
  ( posixCBits,
  )
where

import Asterius.Types
import qualified Data.Map.Strict as M
import System.Posix.Internals

posixCBits :: AsteriusModule
posixCBits = posixOFlags

posixOFlags :: AsteriusModule
posixOFlags =
  mempty
    { functionMap =
        M.fromList
          [ ( k,
              Function
                { functionType =
                    FunctionType
                      { paramTypes = [],
                        returnTypes = [I64]
                      },
                  varTypes = [],
                  body = ConstI64 $ fromIntegral v
                }
            )
            | (k, v) <-
                [ ("__hscore_o_rdonly", o_RDONLY),
                  ("__hscore_o_wronly", o_WRONLY),
                  ("__hscore_o_rdwr", o_RDWR),
                  ("__hscore_o_append", o_APPEND),
                  ("__hscore_o_creat", o_CREAT),
                  ("__hscore_o_excl", o_EXCL),
                  ("__hscore_o_trunc", o_TRUNC),
                  ("__hscore_o_noctty", o_NOCTTY),
                  ("__hscore_o_nonblock", o_NONBLOCK),
                  ("__hscore_o_binary", o_BINARY)
                ]
          ]
    }
