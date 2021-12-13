{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.Binary.TH
  ( genBinary,
  )
where

import qualified Binary as GHC
import Data.Foldable
import Data.Word
import "template-haskell" Language.Haskell.TH

genBinary :: Name -> Q [Dec]
genBinary ty = do
  TyConI dec <- reify ty
  case dec of
    DataD [] ((== ty) -> True) [] Nothing (zip [0 ..] -> cons) _
      | length cons <= 0xFF ->
        pure
          [ InstanceD
              Nothing
              []
              (AppT (ConT ''GHC.Binary) (ConT ty))
              [ FunD
                  'GHC.put_
                  [ Clause
                      [ VarP $ mkName "bh",
                        ConP
                          con_name
                          [VarP $ mkName $ "a" <> show j | j <- [1 .. con_fields]]
                      ]
                      ( NormalB
                          $ DoE
                          $ map
                            ( NoBindS
                                . AppE (AppE (VarE 'GHC.put_) (VarE $ mkName "bh"))
                            )
                          $ [SigE (LitE (IntegerL i)) (ConT ''Word8) | length cons > 1]
                            <> [VarE $ mkName $ "a" <> show j | j <- [1 .. con_fields]]
                      )
                      []
                    | (i, con) <- cons,
                      let con_name = dataConName con
                          con_fields = dataConFields con
                  ],
                FunD
                  'GHC.get
                  [ Clause
                      [VarP $ mkName "bh"]
                      ( NormalB $
                          case length cons of
                            1 ->
                              DoE $
                                [ BindS
                                    (VarP $ mkName $ "a" <> show j)
                                    (AppE (VarE 'GHC.get) (VarE $ mkName "bh"))
                                  | j <- [1 .. con_fields]
                                ]
                                  <> [ NoBindS
                                         $ AppE (VarE 'pure)
                                         $ foldl'
                                           AppE
                                           (ConE con_name)
                                           [ VarE $ mkName $ "a" <> show j
                                             | j <- [1 .. con_fields]
                                           ]
                                     ]
                              where
                                [(0, con)] = cons
                                con_name = dataConName con
                                con_fields = dataConFields con
                            _ ->
                              DoE
                                [ BindS
                                    (SigP (VarP (mkName "t")) (ConT ''Word8))
                                    (AppE (VarE 'GHC.get) (VarE $ mkName "bh")),
                                  NoBindS $
                                    CaseE
                                      (VarE $ mkName "t")
                                      [ Match
                                          (LitP (IntegerL i))
                                          ( NormalB
                                              ( DoE $
                                                  [ BindS
                                                      (VarP $ mkName $ "a" <> show j)
                                                      ( AppE
                                                          (VarE 'GHC.get)
                                                          (VarE $ mkName "bh")
                                                      )
                                                    | j <- [1 .. con_fields]
                                                  ]
                                                    <> [ NoBindS
                                                           $ AppE (VarE 'pure)
                                                           $ foldl'
                                                             AppE
                                                             (ConE con_name)
                                                             [ VarE $ mkName $ "a" <> show j
                                                               | j <- [1 .. con_fields]
                                                             ]
                                                       ]
                                              )
                                          )
                                          []
                                        | (i, con) <- cons,
                                          let con_name = dataConName con
                                              con_fields = dataConFields con
                                      ]
                                ]
                      )
                      []
                  ]
              ]
          ]
    _ -> fail $ "Asterius.Binary.TH.genBinary: " <> show dec

dataConName :: Con -> Name
dataConName (NormalC n _) = n
dataConName (RecC n _) = n
dataConName c = error $ "Asterius.Binary.TH.dataConName: " <> show c

dataConFields :: Con -> Int
dataConFields (NormalC _ fs) = length fs
dataConFields (RecC _ fs) = length fs
dataConFields c = error $ "Asterius.Binary.TH.dataConFields: " <> show c
