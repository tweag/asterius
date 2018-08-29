{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Tracing
  ( addTracingModule
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Char
import Data.Data (Data, gmapT)
import qualified Data.Map.Strict as M
import Foreign
import Type.Reflection

addTracingModule ::
     M.Map AsteriusEntitySymbol Int64
  -> AsteriusEntitySymbol
  -> FunctionType
  -> Function
  -> Function
addTracingModule func_sym_map func_sym func_type func
  | "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName func_sym) = func
  | otherwise = f func
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
                              { target' = "__asterius_traceCmm"
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
                              M.fromList
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
                                                                  "__asterius_traceCmmBlock"
                                                              , operands =
                                                                  [ func_idx
                                                                  , lbl_to_idx
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
                                                                  "__asterius_traceCmmBlock"
                                                              , operands =
                                                                  [ func_idx
                                                                  , lbl_to_idx
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
                                    M.toList blockMap
                                ]
                          }
                    }
                  where lbl_to_idx =
                          ConstI32 .
                          read . map (chr . fromIntegral) . SBS.unpack
                SetLocal {..}
                  | ((index_int < param_num) && (params !! index_int == I64)) ||
                      ((index_int >= param_num) &&
                       varTypes func !! (index_int - param_num) == I64) ->
                    Block
                      { name = ""
                      , bodys =
                          [ SetLocal {index = index, value = value}
                          , CallImport
                              { target' = "__asterius_traceCmmSetLocal"
                              , operands =
                                  [func_idx, ConstI32 $ fromIntegral index] <>
                                  cutI64
                                    GetLocal {index = index, valueType = I64}
                              , valueType = None
                              }
                          ]
                      , valueType = None
                      }
                  where params = paramTypes func_type
                        param_num = length params
                        index_int = fromIntegral index
                _ -> go
            _ -> go
      where
        go = gmapT f x
        func_idx = ConstI32 $ fromIntegral $ func_sym_map ! func_sym
