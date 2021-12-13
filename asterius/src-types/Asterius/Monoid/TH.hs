{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.Monoid.TH
  ( genMonoid,
  )
where

import Data.List (foldl')
import "template-haskell" Language.Haskell.TH

-- | Generate a 'Monoid' instance of the form
--
-- > instance Monoid TyCon where
-- >   mempty = DataCon mempty ... mempty
--
-- Note that this approach works only for monomorphic datatypes with a single
-- data constructor, whose fields are themselves all instances of 'Monoid'.
genMonoid :: Name -> Q [Dec]
genMonoid ty = do
  TyConI dec <- reify ty
  case dec of
    DataD [] ((== ty) -> True) [] Nothing [con] _ ->
      pure
        [ InstanceD
            Nothing
            []
            (AppT (ConT ''Monoid) (ConT ty))
            [ FunD
                'mempty
                [ Clause
                    []
                    ( NormalB $
                        foldl'
                          AppE
                          (ConE $ dataConName con)
                          [VarE 'mempty | _ <- [1 .. dataConFields con]]
                    )
                    []
                ]
            ]
        ]
    _ -> fail $ "Asterius.Monoid.TH.genMonoid: " <> show dec

dataConName :: Con -> Name
dataConName (NormalC n _) = n
dataConName (RecC n _) = n
dataConName c = error $ "Asterius.Monoid.TH.dataConName: " <> show c

dataConFields :: Con -> Int
dataConFields (NormalC _ fs) = length fs
dataConFields (RecC _ fs) = length fs
dataConFields c = error $ "Asterius.Monoid.TH.dataConFields: " <> show c
