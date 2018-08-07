{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

module Splices
  ( genStaticLookupFunction
  ) where

import Language.Haskell.TH

genStaticLookupFunction :: [(Int, Int)] -> ExpQ
genStaticLookupFunction tbl = do
  x <- newName "x"
  pure $
    LamE [VarP x] $
    CaseE (VarE x) $
    [ Match
      (LitP $ IntegerL $ fromIntegral i)
      (NormalB $ LitE $ IntegerL $ fromIntegral o)
      []
    | (i, o) <- tbl
    ] <>
    [Match WildP (NormalB $ VarE $ mkName "undefined") []]
