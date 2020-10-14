{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Asterius.Passes.FindCCall
  ( findCCall,
  )
where

import Asterius.Internals.SYB
import Asterius.Types
import qualified Asterius.Types.SymbolSet as SS
import qualified Data.ByteString.Char8 as CBS
import Type.Reflection

findCCall :: GenericQ [String]
findCCall = map (CBS.unpack . entityName) . SS.toList . everything f
  where
    f :: GenericQ SS.SymbolSet
    f t
      | Just HRefl <- eqTypeRep (typeOf t) (typeRep @Expression) = case t of
        Call {callHint = Just _, ..} -> SS.singleton target
        _ -> SS.empty
      | otherwise = SS.empty
