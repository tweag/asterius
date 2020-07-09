{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.Semigroup.TH
  ( genSemigroup,
  )
where

import Data.List (foldl')
import Language.Haskell.TH

genSemigroup :: Name -> Q [Dec]
genSemigroup ty = do
  TyConI dec <- reify ty
  case dec of
    DataD [] ((== ty) -> True) [] Nothing [con] _ ->
      pure
        [ InstanceD
            Nothing
            []
            (AppT (ConT ''Semigroup) (ConT ty))
            [ FunD
                '(<>)
                [ let avars = [mkName $ "a" <> show j | j <- [1 .. dataConFields con]]
                      bvars = [mkName $ "b" <> show j | j <- [1 .. dataConFields con]]
                   in Clause
                        [ ConP (dataConName con) (map VarP avars),
                          ConP (dataConName con) (map VarP bvars)
                        ]
                        ( NormalB $
                            foldl'
                              (\acc (a, b) -> AppE acc (AppE (AppE (VarE '(<>)) (VarE a)) (VarE b)))
                              (ConE $ dataConName con)
                              (zip avars bvars)
                        )
                        []
                ]
            ]
        ]
    _ -> fail $ "Asterius.Semigroup.TH.genSemigroup: " <> show dec

dataConName :: Con -> Name
dataConName (NormalC n _) = n
dataConName (RecC n _) = n
dataConName c = error $ "Asterius.Semigroup.TH.dataConName: " <> show c

dataConFields :: Con -> Int
dataConFields (NormalC _ fs) = length fs
dataConFields (RecC _ fs) = length fs
dataConFields c = error $ "Asterius.Semigroup.TH.dataConFields: " <> show c
