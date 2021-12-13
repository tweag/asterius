{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.NFData.TH
  ( genNFData,
  )
where

import Control.DeepSeq
import Data.List (foldl1')
import "template-haskell" Language.Haskell.TH

genNFData :: Name -> Q [Dec]
genNFData ty = do
  TyConI dec <- reify ty
  case dec of
    DataD [] ((== ty) -> True) [] Nothing cons _ ->
      pure
        [ InstanceD
            Nothing
            []
            (AppT (ConT ''NFData) (ConT ty))
            [ FunD
                'rnf
                [ Clause
                    [ ConP
                        (dataConName con)
                        (map VarP vars)
                    ]
                    ( NormalB $
                        if null vars
                          then ConE '()
                          else
                            foldl1'
                              (\acc x -> AppE (AppE (VarE 'seq) acc) x)
                              (map (AppE (VarE 'rnf) . VarE) vars)
                    )
                    []
                  | con <- cons,
                    let vars = [mkName $ "a" <> show j | j <- [1 .. dataConFields con]]
                ]
            ]
        ]
    _ -> fail $ "Asterius.NFData.TH.genNFData: " <> show dec

dataConName :: Con -> Name
dataConName (NormalC n _) = n
dataConName (RecC n _) = n
dataConName c = error $ "Asterius.NFData.TH.dataConName: " <> show c

dataConFields :: Con -> Int
dataConFields (NormalC _ fs) = length fs
dataConFields (RecC _ fs) = length fs
dataConFields c = error $ "Asterius.NFData.TH.dataConFields: " <> show c
