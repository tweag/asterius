{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.SM
  ( smCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import Language.Haskell.GHC.Toolkit.Constants

smCBits :: AsteriusModule
smCBits = growStack

growStack :: AsteriusModule
growStack = runEDSL "growStack" $ do
  setReturnTypes [I64]
  prev_stack_obj <- param I64
  prev_sp <- i64Local $ loadI64 prev_stack_obj offset_StgStack_sp
  prev_stack_obj_size <-
    i64Local
      $ addInt64 (constI64 offset_StgStack_stack)
      $ mulInt64 (constI64 8)
      $ extendUInt32
      $ loadI32 prev_stack_obj offset_StgStack_stack_size
  prev_stack_used <-
    i64Local $
      prev_stack_obj_size
        `subInt64` (prev_sp `subInt64` prev_stack_obj)
  next_stack_obj_size <- i64Local $ prev_stack_obj_size `mulInt64` constI64 2
  next_stack_obj <-
    call'
      "allocatePinned"
      [mainCapability, next_stack_obj_size]
      I64
  next_sp <-
    i64Local $
      next_stack_obj
        `addInt64` next_stack_obj_size
        `subInt64` prev_stack_used
  _ <-
    call'
      "memcpy"
      [next_stack_obj, prev_stack_obj, constI64 offset_StgStack_stack]
      I64
  storeI32 next_stack_obj offset_StgStack_stack_size
    $ wrapInt64
    $ (next_stack_obj_size `subInt64` constI64 offset_StgStack_stack)
      `divUInt64` constI64 8
  storeI64 next_stack_obj offset_StgStack_sp next_sp
  _ <- call' "memcpy" [next_sp, prev_sp, prev_stack_used] I64
  emit next_stack_obj
