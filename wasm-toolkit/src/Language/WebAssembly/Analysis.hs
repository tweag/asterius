{-# LANGUAGE GADTs #-}

module Language.WebAssembly.Analysis
  ( collectUnresolvedLabels
  ) where

import Data.Data (Data, gmapQr)
import qualified Data.HashSet as HS
import Language.WebAssembly.Types
import Type.Reflection ((:~~:)(..), TypeRep, eqTypeRep, typeOf, typeRep)

collectUnresolvedLabels :: Data a => a -> HS.HashSet UnresolvedSymbol
collectUnresolvedLabels =
  gmapQr
    (<>)
    mempty
    (\t ->
       case eqTypeRep (typeOf t) (typeRep :: TypeRep UnresolvedSymbol) of
         Just HRefl -> HS.singleton t
         _ -> collectUnresolvedLabels t)
