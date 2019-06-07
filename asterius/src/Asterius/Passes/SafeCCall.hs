{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Passes.SafeCCall
  ( BlockChunk(..)
  , splitBlockBody
  ) where

import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Foldable

data BlockChunk
  = Regular [Expression]
  | SafeCCall AsteriusEntitySymbol
              FFIImportDecl
              Expression

splitBlockBody :: FFIMarshalState -> Expression -> [BlockChunk]
splitBlockBody FFIMarshalState {..} instrs = foldr' w [] es
  where
    get_safe t =
      case AsteriusEntitySymbol . SBS.toShort <$>
           BS.stripSuffix "_wrapper" (SBS.fromShort (entityName t)) of
        Just k
          | ffiSafety imp_decl /= FFIUnsafe -> Just (k, imp_decl)
          where imp_decl = ffiImportDecls ! k
        _ -> Nothing
    add_regular instr (Regular es':cs) = Regular (instr : es') : cs
    add_regular instr cs = Regular [instr] : cs
    w instr@Call {..} cs =
      case get_safe target of
        Just (k, imp_decl) -> SafeCCall k imp_decl instr : cs
        _ -> add_regular instr cs
    w instr@UnresolvedSetLocal {value = Call {..}} cs =
      case get_safe target of
        Just (k, imp_decl) -> SafeCCall k imp_decl instr : cs
        _ -> add_regular instr cs
    w instr cs = add_regular instr cs
    es =
      case instrs of
        Block {..} -> bodys
        Nop -> []
        _ -> [instrs]
