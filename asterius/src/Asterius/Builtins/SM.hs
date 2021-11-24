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
  setReturnTypes [I32]
  prev_stack_obj <- param I32
  prev_sp <- local I32 $ loadI32 prev_stack_obj offset_StgStack_sp
  prev_stack_obj_size <-
    local I32
      $ addInt32 (constI32 offset_StgStack_stack)
      $ mulInt32 (constI32 4)
      $ loadI32 prev_stack_obj offset_StgStack_stack_size
  prev_stack_used <-
    local I32 $
      prev_stack_obj_size
        `subInt32` (prev_sp `subInt32` prev_stack_obj)
  next_stack_obj_size <- local I32 $ prev_stack_obj_size `mulInt32` constI32 2
  next_stack_obj <-
    call'
      "allocatePinned"
      [mainCapability, next_stack_obj_size]
      I32
  next_sp <-
    local I32 $
      next_stack_obj
        `addInt32` next_stack_obj_size
        `subInt32` prev_stack_used
  emit $ memcpy next_stack_obj prev_stack_obj (constI32 offset_StgStack_stack)
  storeI32 next_stack_obj offset_StgStack_stack_size
    $ (next_stack_obj_size `subInt32` constI32 offset_StgStack_stack)
      `divUInt32` constI32 4
  storeI32 next_stack_obj offset_StgStack_sp next_sp
  emit $ memcpy next_sp prev_sp prev_stack_used
  emit next_stack_obj
