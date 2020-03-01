{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Main
  ( mainBuiltins,
  )
where

import Asterius.Types
import qualified Data.Map.Strict as M
import Language.Haskell.GHC.Toolkit.Constants

mainBuiltins :: AsteriusModule
mainBuiltins =
  mempty
    { staticsMap =
        M.singleton
          "main_closure"
          AsteriusStatics
            { staticsType = Closure,
              asteriusStatics =
                [ SymbolStatic "stg_ap_2_upd_info" 0,
                  Uninitialized $ offset_StgThunk_payload - 8,
                  SymbolStatic "base_AsteriusziTopHandler_runIO_closure" 0,
                  SymbolStatic "Main_main_closure" 0
                ]
            },
      ffiMarshalState =
        mempty
          { ffiExportDecls =
              M.singleton
                "main"
                FFIExportDecl
                  { ffiFunctionType =
                      FFIFunctionType
                        { ffiParamTypes = [],
                          ffiResultTypes = [],
                          ffiInIO = True
                        },
                    ffiExportClosure = "main_closure"
                  }
          }
    }
