{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Tracing
  ( addTracingModule
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.TypeInfer
import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Char
import Data.Data (Data, gmapM)
import qualified Data.Map.Strict as M
import Data.Traversable
import Foreign
import Type.Reflection

addTracingModule ::
     Monad m
  => M.Map AsteriusEntitySymbol Int64
  -> AsteriusEntitySymbol
  -> FunctionType
  -> Function
  -> m Function
addTracingModule func_sym_map func_sym func_type func
  | "__asterius" `BS.isPrefixOf` SBS.fromShort (entityName func_sym) = pure func
  | otherwise = f func
  where
    f :: (Monad m, Data a) => a -> m a
    f x =
      case eqTypeRep (typeOf x) (typeRep :: TypeRep Function) of
        Just HRefl ->
          case x of
            Function {..} -> do
              new_body <- f body
              pure
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
                            , new_body
                            ]
                        , valueType = infer new_body
                        }
                  }
        _ ->
          case eqTypeRep (typeOf x) (typeRep :: TypeRep Expression) of
            Just HRefl ->
              case x of
                CFG {graph = g@RelooperRun {..}} -> do
                  new_block_map <-
                    fmap M.fromList $
                    for (M.toList blockMap) $ \(lbl, blk@RelooperBlock {..}) ->
                      case addBlock of
                        AddBlock {..} -> do
                          new_code <- f code
                          pure
                            ( lbl
                            , blk
                                { addBlock =
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
                                                        , lbl_to_idx lbl
                                                        ]
                                                    , valueType = None
                                                    }
                                                , new_code
                                                ]
                                            , valueType = infer new_code
                                            }
                                      }
                                })
                        AddBlockWithSwitch {..} -> do
                          new_code <- f code
                          pure
                            ( lbl
                            , blk
                                { addBlock =
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
                                                        , lbl_to_idx lbl
                                                        ]
                                                    , valueType = None
                                                    }
                                                , new_code
                                                ]
                                            , valueType = infer new_code
                                            }
                                      , condition = condition
                                      }
                                })
                  pure CFG {graph = g {blockMap = new_block_map}}
                  where lbl_to_idx =
                          ConstI32 .
                          read . map (chr . fromIntegral) . SBS.unpack
                SetLocal {..}
                  | ((index_int < param_num) && (params !! index_int == I64)) ||
                      ((index_int >= param_num) &&
                       varTypes func !! (index_int - param_num) == I64) ->
                    pure
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
        go = gmapM f x
        func_idx = ConstI32 $ fromIntegral $ func_sym_map ! func_sym
