{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.Relooper
  ( relooper,
  )
where

import Asterius.Internals.SafeFromIntegral
import Asterius.Types
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M

{-# INLINE relooper #-}
relooper :: RelooperRun -> Expression
relooper cfg@RelooperRun {blockMap = oldMap} =
  relooper' $
    cfg
      { blockMap = M.insert "__asterius_unreachable" unreachableRelooperBlock oldMap
      }

relooper' :: RelooperRun -> Expression
relooper' RelooperRun {..} = result_expr
  where
    lbls = M.keys blockMap
    lbl_map = M.fromList $ zip lbls [0 ..]
    lbl_to_idx = (lbl_map M.!)
    set_block_lbl lbl = SetLocal {index = 0, value = ConstI32 $ lbl_to_idx lbl}
    initial_expr = Switch
      { names = lbls,
        defaultName = "__asterius_unreachable",
        condition = GetLocal {index = 0, valueType = I32}
      }
    loop_lbl = "__asterius_loop"
    exit_lbl = "__asterius_exit"
    (blocks_expr, last_block_residule_exprs) =
      M.foldlWithKey'
        ( \(tot_expr, residule_exprs) lbl RelooperBlock {..} ->
            ( Block
                { name = lbl,
                  bodys = tot_expr : residule_exprs,
                  blockReturnTypes = []
                },
              case addBlock of
                AddBlock {..} ->
                  code
                    : ( case addBranches of
                          [] -> [Break {name = exit_lbl, breakCondition = Nothing}]
                          branches ->
                            foldr'
                              ( \AddBranch {addBranchCondition = Just cond, to} e -> If
                                  { condition = cond,
                                    ifTrue = set_block_lbl to,
                                    ifFalse = Just e
                                  }
                              )
                              (set_block_lbl $ to def_branch)
                              (init branches)
                              : [Break {name = loop_lbl, breakCondition = Nothing}]
                            where
                              def_branch@AddBranch {addBranchCondition = Nothing} =
                                last branches
                      )
                AddBlockWithSwitch {..} ->
                  [code, SetLocal {index = 1, value = condition}]
                    <> ( foldr'
                           ( \AddBranchForSwitch {to, indexes} e -> If
                               { condition =
                                   foldl1'
                                     (Binary OrInt32)
                                     [ Binary
                                         { binaryOp = EqInt32,
                                           operand0 = GetLocal
                                             { index = 1,
                                               valueType = I32
                                             },
                                           operand1 = ConstI32 $ safeFromIntegral tag
                                         }
                                       | tag <- indexes
                                     ],
                                 ifTrue = set_block_lbl to,
                                 ifFalse = Just e
                               }
                           )
                           (set_block_lbl $ to def_branch)
                           (init branches)
                           : [Break {name = loop_lbl, breakCondition = Nothing}]
                       )
                  where
                    branches = addBranches
                    def_branch@AddBranch {addBranchCondition = Nothing} = last branches
            )
        )
        (initial_expr, [])
        blockMap
    result_expr = Block
      { name = exit_lbl,
        bodys =
          [ set_block_lbl entry,
            Loop
              { name = loop_lbl,
                body = Block
                  { name = "",
                    bodys = blocks_expr : last_block_residule_exprs,
                    blockReturnTypes = []
                  }
              }
          ],
        blockReturnTypes = []
      }
