{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Tracing
  ( addTracingModule
  ) where

import Asterius.Types
import Data.Data (Data, gmapT)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Foreign
import Type.Reflection

addTracingModule ::
     Data a
  => HM.HashMap AsteriusEntitySymbol Int64
  -> AsteriusEntitySymbol
  -> a
  -> a
addTracingModule func_sym_map func_sym = f
  where
    f :: Data a => a -> a
    f x =
      case eqTypeRep (typeOf x) (typeRep :: TypeRep Function) of
        Just HRefl ->
          case x of
            Function {..} ->
              Function
                { functionTypeName = functionTypeName
                , varTypes = varTypes
                , body =
                    if func_sym `V.elem`
                       ["_get_Sp", "_get_SpLim", "_get_Hp", "_get_HpLim"]
                      then body
                      else Block
                             { name = ""
                             , bodys =
                                 [ CallImport
                                     { target' = "traceCmm"
                                     , operands =
                                         [ ConstI32 $
                                           fromIntegral $
                                           func_sym_map HM.! func_sym
                                         ]
                                     , valueType = None
                                     }
                                 , body
                                 ]
                             , valueType = Auto
                             }
                }
        _ -> go
      where
        go = gmapT f x
