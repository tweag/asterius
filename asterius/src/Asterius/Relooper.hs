{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Relooper
  ( relooper
  ) where

import Asterius.Types
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Vector as V

relooper :: RelooperRun -> Expression
relooper RelooperRun {..} = result_expr
  where
    lbls = sort $ HM.keys blockMap
    lbl_map = HM.fromList $ zip lbls [0 ..]
    lbl_to_idx = (lbl_map HM.!)
    set_block_lbl lbl = SetLocal {index = 0, value = ConstI32 $ lbl_to_idx lbl}
    initial_expr =
      Switch
        { names = V.fromList lbls
        , defaultName = "__asterius_unreachable"
        , condition = GetLocal {index = 0, valueType = I32}
        , value = Null
        }
    loop_lbl = "__asterius_loop"
    exit_lbl = "__asterius_exit"
    (blocks_expr, last_block_residule_exprs) =
      HM.foldlWithKey'
        (\(tot_expr, residule_exprs) lbl RelooperBlock {..} ->
           ( Block
               { name = lbl
               , bodys = V.fromList $ tot_expr : residule_exprs
               , valueType = None
               }
           , case addBlock of
               AddBlock {..} ->
                 code :
                 (case V.toList addBranches of
                    [] ->
                      [Break {name = exit_lbl, condition = Null, value = Null}]
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
                      [Break {name = loop_lbl, condition = Null, value = Null}]
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
                               | tag <- V.toList indexes
                               ]
                         , ifTrue = set_block_lbl to
                         , ifFalse = e
                         })
                    (set_block_lbl $ to def_branch)
                    (init branches) :
                  [Break {name = loop_lbl, condition = Null, value = Null}])
                 where branches = V.toList addBranches
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
                      , bodys =
                          V.fromList $ blocks_expr : last_block_residule_exprs
                      , valueType = None
                      }
                }
            ]
        , valueType = None
        }
