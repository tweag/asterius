{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Main
  ( mainBuiltins,
  )
where

import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM

mainBuiltins :: AsteriusCachedModule
mainBuiltins =
  mempty
    { asteriusModule =
        mempty
          { ffiMarshalState =
              mempty
                { ffiExportDecls =
                    SM.singleton
                      "main"
                      FFIExportDecl
                        { ffiFunctionType =
                            FFIFunctionType
                              { ffiParamTypes = [],
                                ffiResultTypes = [],
                                ffiInIO = True
                              },
                          ffiExportClosure = "Main_main_closure"
                        }
                }
          }
    }
