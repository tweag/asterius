{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.Tracing
  ( traceModule,
  )
where

import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import qualified Data.ByteString.Char8 as CBS
import Data.Char

traceModule :: AsteriusModule -> AsteriusModule
traceModule m = m {functionMap = SM.mapWithKey traceFunction (functionMap m)}

traceFunction :: EntitySymbol -> Function -> Function
traceFunction sym Function {..} =
  Function
    { functionType = functionType,
      varTypes = varTypes,
      body =
        Block
          { name = "",
            bodys =
              [ CallImport
                  { target' = "barf_push",
                    operands = [ConstI32 $ fromIntegral $ ord c],
                    callImportReturnTypes = []
                  }
                | c <- CBS.unpack (entityName sym)
              ]
                <> [ CallImport
                       { target' = "barf_signal",
                         operands = [ConstI32 0],
                         callImportReturnTypes = []
                       },
                     body
                   ],
            blockReturnTypes = returnTypes functionType
          }
    }
