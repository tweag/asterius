{-# LANGUAGE GADTs #-}

module Asterius.Analysis
  ( collectUnresolvedSymbols
  ) where

import Data.Data (Data, gmapQr)
import qualified Data.HashSet as HS
import Asterius.Types
import Type.Reflection ((:~~:)(..), TypeRep, eqTypeRep, typeOf, typeRep)

collectUnresolvedSymbols :: Data a => a -> HS.HashSet UnresolvedSymbol
collectUnresolvedSymbols =
  gmapQr
    (<>)
    mempty
    (\t ->
       case eqTypeRep (typeOf t) (typeRep :: TypeRep UnresolvedSymbol) of
         Just HRefl -> HS.singleton t
         _ -> collectUnresolvedSymbols t)
