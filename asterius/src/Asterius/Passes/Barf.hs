{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.Barf
  ( processBarf,
  )
where

import Asterius.Types
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Short as SBS
import Data.Data
  ( Data,
    gmapM,
  )
import qualified Data.Map.Strict as M
import Data.String
import Data.Word
import qualified Encoding as GHC
import Type.Reflection

processBarf :: EntitySymbol -> Function -> AsteriusModule
processBarf sym f =
  mempty
    { staticsMap = sm,
      functionMap = M.singleton sym f'
    }
  where
    (f', (_, sm)) = runState (w f) (0, M.empty)
    w ::
      Data a =>
      a ->
      State (Word64, M.Map EntitySymbol AsteriusStatics) a
    w t = case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
      Just HRefl -> case t of
        Barf {..} -> do
          (i, sm_acc) <- get
          let i_sym = fromString $ p <> GHC.toBase62 i
              ss = AsteriusStatics
                { staticsType = ConstBytes,
                  asteriusStatics =
                    [ Serialized
                        $ fromString
                        $ sym_str
                          <> ": "
                          <> unpack barfMessage
                          <> "\0"
                    ]
                }
          put (succ i, M.insert i_sym ss sm_acc)
          pure Block
            { name = SBS.empty,
              bodys =
                [ Call
                    { target = "barf",
                      operands =
                        [ Symbol
                            { unresolvedSymbol = i_sym,
                              symbolOffset = 0
                            }
                        ],
                      callReturnTypes = []
                    },
                  Unreachable
                ],
              blockReturnTypes = barfReturnTypes
            }
        _ -> go
      _ -> go
      where
        go = gmapM w t
    sym_str = unpack (entityName sym)
    p = "__asterius_barf_" <> sym_str <> "_"
    unpack = CBS.unpack . SBS.fromShort
