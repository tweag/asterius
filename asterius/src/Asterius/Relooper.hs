{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Relooper
  ( relooper
  , relooperDeep
  ) where

import Asterius.Internals
import Asterius.Types
import Data.Data (Data, gmapM)
import Data.List
import qualified Data.Map.Strict as M
import Data.String
import Type.Reflection

relooper :: RelooperRun -> Expression
relooper RelooperRun {..} = result_expr
  where
    lbls = M.keys blockMap
    def_lbl = fromString $ show $ length lbls - 1
    lbl_map = M.fromList $ zip lbls [0 ..]
    lbl_to_idx = (lbl_map !)
    set_block_lbl lbl = SetLocal {index = 0, value = ConstI32 $ lbl_to_idx lbl}
    initial_expr =
      Switch
        { names = lbls
        , defaultName = def_lbl
        , condition = GetLocal {index = 0, valueType = I32}
        }
    loop_lbl = "__asterius_loop"
    exit_lbl = "__asterius_exit"
    (blocks_expr, last_block_residule_exprs) =
      M.foldlWithKey'
        (\(tot_expr, residule_exprs) lbl RelooperBlock {..} ->
           ( Block
               {name = lbl, bodys = tot_expr : residule_exprs, valueType = None}
           , case addBlock of
               AddBlock {..} ->
                 code :
                 (case addBranches of
                    [] -> [Break {name = exit_lbl, condition = Null}]
                    branches ->
                      foldr
                        (\AddBranch {condition, to} e ->
                           If
                             { condition = condition
                             , ifTrue = set_block_lbl to
                             , ifFalse = e
                             })
                        (set_block_lbl $ to def_branch)
                        (init branches) :
                      [Break {name = loop_lbl, condition = Null}]
                      where def_branch@AddBranch {condition = Null, code = Null} =
                              last branches)
               AddBlockWithSwitch {..} ->
                 [code, SetLocal {index = 1, value = condition}] <>
                 (foldr
                    (\AddBranchForSwitch {to, indexes} e ->
                       If
                         { condition =
                             foldl1'
                               (Binary OrInt32)
                               [ Binary
                                 { binaryOp = EqInt32
                                 , operand0 =
                                     GetLocal {index = 1, valueType = I32}
                                 , operand1 = ConstI32 $ fromIntegral tag
                                 }
                               | tag <- indexes
                               ]
                         , ifTrue = set_block_lbl to
                         , ifFalse = e
                         })
                    (set_block_lbl $ to def_branch)
                    (init branches) :
                  [Break {name = loop_lbl, condition = Null}])
                 where branches = addBranches
                       def_branch@AddBranch {condition = Null, code = Null} =
                         last branches))
        (initial_expr, [])
        blockMap
    result_expr =
      Block
        { name = exit_lbl
        , bodys =
            [ SetLocal {index = 0, value = ConstI32 $ lbl_to_idx entry}
            , Loop
                { name = loop_lbl
                , body =
                    Block
                      { name = ""
                      , bodys = blocks_expr : last_block_residule_exprs
                      , valueType = None
                      }
                }
            ]
        , valueType = None
        }

relooperDeep :: (Monad m, Data a) => a -> m a
relooperDeep t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        CFG {..} -> pure $ relooper graph
        _ -> go
    _ -> go
  where
    go = gmapM relooperDeep t
