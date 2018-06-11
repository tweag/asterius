{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Tracing
  ( addTracingModule
  ) where

import Asterius.Builtins
import Asterius.MemoryTrap
import Asterius.Types
import Data.Data (Data, gmapT)
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Vector as V
import Foreign
import Type.Reflection

addTracingModule ::
     HM.HashMap AsteriusEntitySymbol Int64
  -> AsteriusEntitySymbol
  -> Function
  -> Function
addTracingModule func_sym_map func_sym func
  | func_sym `V.elem`
      [ "_get_Sp"
      , "_get_SpLim"
      , "_get_Hp"
      , "_get_HpLim"
      , "__asterius_memory_trap"
      ] = func
  | otherwise = addMemoryTrap $ f func
  where
    f :: Data a => a -> a
    f x =
      case eqTypeRep (typeOf x) (typeRep :: TypeRep Function) of
        Just HRefl ->
          case x of
            Function {..} ->
              Function
                { functionTypeName = functionTypeName
                , varTypes = varTypes
                , body =
                    Block
                      { name = ""
                      , bodys =
                          [ CallImport
                              { target' = "traceCmm"
                              , operands = [func_idx]
                              , valueType = None
                              }
                          , f body
                          ]
                      , valueType = Auto
                      }
                }
        _ ->
          case eqTypeRep (typeOf x) (typeRep :: TypeRep Expression) of
            Just HRefl ->
              case x of
                CFG {graph = g@RelooperRun {..}} ->
                  CFG
                    { graph =
                        g
                          { blockMap =
                              HM.fromList
                                [ ( lbl
                                  , blk
                                      { addBlock =
                                          case addBlock of
                                            AddBlock {..} ->
                                              AddBlock
                                                { code =
                                                    Block
                                                      { name = ""
                                                      , bodys =
                                                          [ CallImport
                                                              { target' =
                                                                  "traceCmmBlock"
                                                              , operands =
                                                                  [ lbl_to_idx
                                                                      lbl
                                                                  ]
                                                              , valueType = None
                                                              }
                                                          , f code
                                                          ]
                                                      , valueType = Auto
                                                      }
                                                }
                                            AddBlockWithSwitch {..} ->
                                              AddBlockWithSwitch
                                                { code =
                                                    Block
                                                      { name = ""
                                                      , bodys =
                                                          [ CallImport
                                                              { target' =
                                                                  "traceCmmBlock"
                                                              , operands =
                                                                  [ lbl_to_idx
                                                                      lbl
                                                                  ]
                                                              , valueType = None
                                                              }
                                                          , f code
                                                          ]
                                                      , valueType = Auto
                                                      }
                                                , condition = condition
                                                }
                                      })
                                | (lbl, blk@RelooperBlock {..}) <-
                                    HM.toList blockMap
                                ]
                          }
                    }
                  where lbls = sort $ HM.keys blockMap
                        lbl_to_idx lbl = ConstI32 $ fromIntegral idx
                          where
                            Just idx = elemIndex lbl lbls
                SetLocal {..}
                  | ((index_int < param_num) && (params V.! index_int == I64)) ||
                      ((index_int >= param_num) &&
                       varTypes func V.! (index_int - param_num) == I64) ->
                    Block
                      { name = ""
                      , bodys =
                          [ SetLocal {index = index, value = value}
                          , CallImport
                              { target' = "traceCmmSetLocal"
                              , operands =
                                  [ConstI32 $ fromIntegral index] <>
                                  cutI64
                                    GetLocal {index = index, valueType = I64}
                              , valueType = None
                              }
                          ]
                      , valueType = None
                      }
                  where Function {functionTypeName = ft} = func
                        params = paramTypes (rtsAsteriusFunctionTypeMap HM.! ft)
                        param_num = V.length params
                        index_int = fromIntegral index
                _ -> go
            _ -> go
      where
        go = gmapT f x
        func_idx = ConstI32 $ fromIntegral $ func_sym_map HM.! func_sym
