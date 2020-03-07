module Language.Haskell.GHC.Toolkit.Constants where

foreign import ccall unsafe "offset_timespec_tv_sec"
  offset_timespec_tv_sec ::
    Int

foreign import ccall unsafe "offset_timespec_tv_nsec"
  offset_timespec_tv_nsec ::
    Int

foreign import ccall unsafe "roundup" roundup :: Int -> Int -> Int

foreign import ccall unsafe "roundup_bytes_to_words"
  roundup_bytes_to_words :: Int -> Int

foreign import ccall unsafe "block_size" block_size :: Int

foreign import ccall unsafe "mblock_size" mblock_size :: Int

foreign import ccall unsafe "blocks_per_mblock" blocks_per_mblock :: Int

foreign import ccall unsafe "offset_first_bdescr" offset_first_bdescr :: Int

foreign import ccall unsafe "offset_last_bdescr" offset_last_bdescr :: Int

foreign import ccall unsafe "offset_first_block" offset_first_block :: Int

foreign import ccall unsafe "offset_last_block" offset_last_block :: Int

foreign import ccall unsafe "sizeof_bdescr" sizeof_bdescr :: Int

foreign import ccall unsafe "offset_bdescr_start" offset_bdescr_start :: Int

foreign import ccall unsafe "offset_bdescr_free" offset_bdescr_free :: Int

foreign import ccall unsafe "offset_bdescr_link" offset_bdescr_link :: Int

foreign import ccall unsafe "offset_bdescr_node" offset_bdescr_node :: Int

foreign import ccall unsafe "offset_bdescr_flags" offset_bdescr_flags :: Int

foreign import ccall unsafe "offset_bdescr_blocks" offset_bdescr_blocks :: Int

foreign import ccall unsafe "sizeof_Capability" sizeof_Capability :: Int

foreign import ccall unsafe "offset_Capability_f" offset_Capability_f :: Int

foreign import ccall unsafe "offset_Capability_r" offset_Capability_r :: Int

foreign import ccall unsafe "offset_Capability_no" offset_Capability_no :: Int

foreign import ccall unsafe "offset_Capability_node"
  offset_Capability_node :: Int

foreign import ccall unsafe "offset_Capability_running_task"
  offset_Capability_running_task :: Int

foreign import ccall unsafe "offset_Capability_in_haskell"
  offset_Capability_in_haskell :: Int

foreign import ccall unsafe "offset_Capability_idle"
  offset_Capability_idle :: Int

foreign import ccall unsafe "offset_Capability_disabled"
  offset_Capability_disabled :: Int

foreign import ccall unsafe "offset_Capability_run_queue_hd"
  offset_Capability_run_queue_hd :: Int

foreign import ccall unsafe "offset_Capability_run_queue_tl"
  offset_Capability_run_queue_tl :: Int

foreign import ccall unsafe "offset_Capability_n_run_queue"
  offset_Capability_n_run_queue :: Int

foreign import ccall unsafe "offset_Capability_suspended_ccalls"
  offset_Capability_suspended_ccalls :: Int

foreign import ccall unsafe "offset_Capability_n_suspended_ccalls"
  offset_Capability_n_suspended_ccalls :: Int

foreign import ccall unsafe "offset_Capability_mut_lists"
  offset_Capability_mut_lists :: Int

foreign import ccall unsafe "offset_Capability_saved_mut_lists"
  offset_Capability_saved_mut_lists :: Int

foreign import ccall unsafe "offset_Capability_pinned_object_block"
  offset_Capability_pinned_object_block :: Int

foreign import ccall unsafe "offset_Capability_pinned_object_blocks"
  offset_Capability_pinned_object_blocks :: Int

foreign import ccall unsafe "offset_Capability_weak_ptr_list_hd"
  offset_Capability_weak_ptr_list_hd :: Int

foreign import ccall unsafe "offset_Capability_weak_ptr_list_tl"
  offset_Capability_weak_ptr_list_tl :: Int

foreign import ccall unsafe "offset_Capability_context_switch"
  offset_Capability_context_switch :: Int

foreign import ccall unsafe "offset_Capability_interrupt"
  offset_Capability_interrupt :: Int

foreign import ccall unsafe "offset_Capability_total_allocated"
  offset_Capability_total_allocated :: Int

foreign import ccall unsafe "offset_Capability_free_tvar_watch_queues"
  offset_Capability_free_tvar_watch_queues :: Int

foreign import ccall unsafe "offset_Capability_free_trec_chunks"
  offset_Capability_free_trec_chunks :: Int

foreign import ccall unsafe "offset_Capability_free_trec_headers"
  offset_Capability_free_trec_headers :: Int

foreign import ccall unsafe "offset_Capability_transaction_tokens"
  offset_Capability_transaction_tokens :: Int

foreign import ccall unsafe "sizeof_StgAP" sizeof_StgAP :: Int

foreign import ccall unsafe "offset_StgAP_arity" offset_StgAP_arity :: Int

foreign import ccall unsafe "offset_StgAP_n_args" offset_StgAP_n_args :: Int

foreign import ccall unsafe "offset_StgAP_fun" offset_StgAP_fun :: Int

foreign import ccall unsafe "offset_StgAP_payload" offset_StgAP_payload :: Int

foreign import ccall unsafe "sizeof_StgAP_STACK" sizeof_StgAP_STACK :: Int

foreign import ccall unsafe "offset_StgAP_STACK_size"
  offset_StgAP_STACK_size :: Int

foreign import ccall unsafe "offset_StgAP_STACK_fun"
  offset_StgAP_STACK_fun :: Int

foreign import ccall unsafe "offset_StgAP_STACK_payload"
  offset_StgAP_STACK_payload :: Int

foreign import ccall unsafe "sizeof_StgArrBytes" sizeof_StgArrBytes :: Int

foreign import ccall unsafe "offset_StgArrBytes_bytes"
  offset_StgArrBytes_bytes :: Int

foreign import ccall unsafe "offset_StgArrBytes_payload"
  offset_StgArrBytes_payload :: Int

foreign import ccall unsafe "sizeof_StgClosure" sizeof_StgClosure :: Int

foreign import ccall unsafe "offset_StgClosure_payload"
  offset_StgClosure_payload :: Int

foreign import ccall unsafe "sizeof_StgInd" sizeof_StgInd :: Int

foreign import ccall unsafe "offset_StgInd_indirectee"
  offset_StgInd_indirectee :: Int

foreign import ccall unsafe "sizeof_StgIndStatic" sizeof_StgIndStatic :: Int

foreign import ccall unsafe "offset_StgIndStatic_indirectee"
  offset_StgIndStatic_indirectee :: Int

foreign import ccall unsafe "offset_StgIndStatic_static_link"
  offset_StgIndStatic_static_link :: Int

foreign import ccall unsafe "offset_StgIndStatic_saved_info"
  offset_StgIndStatic_saved_info :: Int

foreign import ccall unsafe "offset_StgFunInfoExtraFwd_fun_type"
  offset_StgFunInfoExtraFwd_fun_type :: Int

foreign import ccall unsafe "offset_StgFunInfoExtraFwd_srt"
  offset_StgFunInfoExtraFwd_srt :: Int

foreign import ccall unsafe "offset_StgFunInfoExtraFwd_b"
  offset_StgFunInfoExtraFwd_b :: Int

foreign import ccall unsafe "offset_StgFunInfoTable_i"
  offset_StgFunInfoTable_i :: Int

foreign import ccall unsafe "offset_StgFunInfoTable_f"
  offset_StgFunInfoTable_f :: Int

foreign import ccall unsafe "sizeof_StgFunTable" sizeof_StgFunTable :: Int

foreign import ccall unsafe "offset_StgFunTable_stgEagerBlackholeInfo"
  offset_StgFunTable_stgEagerBlackholeInfo :: Int

foreign import ccall unsafe "offset_StgFunTable_stgGCEnter1"
  offset_StgFunTable_stgGCEnter1 :: Int

foreign import ccall unsafe "offset_StgFunTable_stgGCFun"
  offset_StgFunTable_stgGCFun :: Int

foreign import ccall unsafe "offset_StgInfoTable_entry"
  offset_StgInfoTable_entry :: Int

foreign import ccall unsafe "offset_StgInfoTable_layout"
  offset_StgInfoTable_layout :: Int

foreign import ccall unsafe "offset_StgInfoTable_type"
  offset_StgInfoTable_type :: Int

foreign import ccall unsafe "offset_StgInfoTable_srt"
  offset_StgInfoTable_srt :: Int

foreign import ccall unsafe "offset_StgLargeBitmap_size"
  offset_StgLargeBitmap_size :: Int

foreign import ccall unsafe "offset_StgLargeBitmap_bitmap"
  offset_StgLargeBitmap_bitmap :: Int

foreign import ccall unsafe "sizeof_StgMutArrPtrs" sizeof_StgMutArrPtrs :: Int

foreign import ccall unsafe "offset_StgMutArrPtrs_ptrs"
  offset_StgMutArrPtrs_ptrs :: Int

foreign import ccall unsafe "offset_StgMutArrPtrs_size"
  offset_StgMutArrPtrs_size :: Int

foreign import ccall unsafe "offset_StgMutArrPtrs_payload"
  offset_StgMutArrPtrs_payload :: Int

foreign import ccall unsafe "offset_StgMVar_head" offset_StgMVar_head :: Int

foreign import ccall unsafe "offset_StgMVar_tail" offset_StgMVar_tail :: Int

foreign import ccall unsafe "offset_StgMVar_value" offset_StgMVar_value :: Int

foreign import ccall unsafe "sizeof_StgPAP" sizeof_StgPAP :: Int

foreign import ccall unsafe "offset_StgPAP_arity" offset_StgPAP_arity :: Int

foreign import ccall unsafe "offset_StgPAP_n_args" offset_StgPAP_n_args :: Int

foreign import ccall unsafe "offset_StgPAP_fun" offset_StgPAP_fun :: Int

foreign import ccall unsafe "offset_StgPAP_payload" offset_StgPAP_payload :: Int

foreign import ccall unsafe "sizeof_StgRetFun" sizeof_StgRetFun :: Int

foreign import ccall unsafe "offset_StgRetFun_size" offset_StgRetFun_size :: Int

foreign import ccall unsafe "offset_StgRetFun_fun" offset_StgRetFun_fun :: Int

foreign import ccall unsafe "offset_StgRetFun_payload"
  offset_StgRetFun_payload :: Int

foreign import ccall unsafe "offset_StgRetInfoTable_i"
  offset_StgRetInfoTable_i :: Int

foreign import ccall unsafe "offset_StgRetInfoTable_srt"
  offset_StgRetInfoTable_srt :: Int

foreign import ccall unsafe "sizeof_StgRegTable" sizeof_StgRegTable :: Int

foreign import ccall unsafe "offset_StgRegTable_rR1"
  offset_StgRegTable_rR1 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR2"
  offset_StgRegTable_rR2 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR3"
  offset_StgRegTable_rR3 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR4"
  offset_StgRegTable_rR4 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR5"
  offset_StgRegTable_rR5 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR6"
  offset_StgRegTable_rR6 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR7"
  offset_StgRegTable_rR7 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR8"
  offset_StgRegTable_rR8 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR9"
  offset_StgRegTable_rR9 :: Int

foreign import ccall unsafe "offset_StgRegTable_rR10"
  offset_StgRegTable_rR10 :: Int

foreign import ccall unsafe "offset_StgRegTable_rF1"
  offset_StgRegTable_rF1 :: Int

foreign import ccall unsafe "offset_StgRegTable_rF2"
  offset_StgRegTable_rF2 :: Int

foreign import ccall unsafe "offset_StgRegTable_rF3"
  offset_StgRegTable_rF3 :: Int

foreign import ccall unsafe "offset_StgRegTable_rF4"
  offset_StgRegTable_rF4 :: Int

foreign import ccall unsafe "offset_StgRegTable_rF5"
  offset_StgRegTable_rF5 :: Int

foreign import ccall unsafe "offset_StgRegTable_rF6"
  offset_StgRegTable_rF6 :: Int

foreign import ccall unsafe "offset_StgRegTable_rD1"
  offset_StgRegTable_rD1 :: Int

foreign import ccall unsafe "offset_StgRegTable_rD2"
  offset_StgRegTable_rD2 :: Int

foreign import ccall unsafe "offset_StgRegTable_rD3"
  offset_StgRegTable_rD3 :: Int

foreign import ccall unsafe "offset_StgRegTable_rD4"
  offset_StgRegTable_rD4 :: Int

foreign import ccall unsafe "offset_StgRegTable_rD5"
  offset_StgRegTable_rD5 :: Int

foreign import ccall unsafe "offset_StgRegTable_rD6"
  offset_StgRegTable_rD6 :: Int

foreign import ccall unsafe "offset_StgRegTable_rL1"
  offset_StgRegTable_rL1 :: Int

foreign import ccall unsafe "offset_StgRegTable_rSp"
  offset_StgRegTable_rSp :: Int

foreign import ccall unsafe "offset_StgRegTable_rSpLim"
  offset_StgRegTable_rSpLim :: Int

foreign import ccall unsafe "offset_StgRegTable_rHp"
  offset_StgRegTable_rHp :: Int

foreign import ccall unsafe "offset_StgRegTable_rHpLim"
  offset_StgRegTable_rHpLim :: Int

foreign import ccall unsafe "offset_StgRegTable_rCCCS"
  offset_StgRegTable_rCCCS :: Int

foreign import ccall unsafe "offset_StgRegTable_rNursery"
  offset_StgRegTable_rNursery :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentTSO"
  offset_StgRegTable_rCurrentTSO :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentNursery"
  offset_StgRegTable_rCurrentNursery :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentAlloc"
  offset_StgRegTable_rCurrentAlloc :: Int

foreign import ccall unsafe "offset_StgRegTable_rHpAlloc"
  offset_StgRegTable_rHpAlloc :: Int

foreign import ccall unsafe "offset_StgRegTable_rRet"
  offset_StgRegTable_rRet :: Int

foreign import ccall unsafe "sizeof_StgSelector" sizeof_StgSelector :: Int

foreign import ccall unsafe "offset_StgSelector_selectee"
  offset_StgSelector_selectee :: Int

foreign import ccall unsafe "sizeof_StgSmallMutArrPtrs"
  sizeof_StgSmallMutArrPtrs :: Int

foreign import ccall unsafe "offset_StgSmallMutArrPtrs_ptrs"
  offset_StgSmallMutArrPtrs_ptrs :: Int

foreign import ccall unsafe "offset_StgSmallMutArrPtrs_payload"
  offset_StgSmallMutArrPtrs_payload :: Int

foreign import ccall unsafe "sizeof_StgStack" sizeof_StgStack :: Int

foreign import ccall unsafe "offset_StgStack_stack_size"
  offset_StgStack_stack_size :: Int

foreign import ccall unsafe "offset_StgStack_dirty" offset_StgStack_dirty :: Int

foreign import ccall unsafe "offset_StgStack_sp" offset_StgStack_sp :: Int

foreign import ccall unsafe "offset_StgStack_stack" offset_StgStack_stack :: Int

foreign import ccall unsafe "sizeof_StgStopFrame" sizeof_StgStopFrame :: Int

foreign import ccall unsafe "sizeof_StgThunk" sizeof_StgThunk :: Int

foreign import ccall unsafe "offset_StgThunk_payload"
  offset_StgThunk_payload :: Int

foreign import ccall unsafe "offset_StgThunkInfoTable_i"
  offset_StgThunkInfoTable_i :: Int

foreign import ccall unsafe "offset_StgThunkInfoTable_srt"
  offset_StgThunkInfoTable_srt :: Int

foreign import ccall unsafe "sizeof_StgTSO" sizeof_StgTSO :: Int

foreign import ccall unsafe "offset_StgTSO__link" offset_StgTSO__link :: Int

foreign import ccall unsafe "offset_StgTSO_stackobj"
  offset_StgTSO_stackobj :: Int

foreign import ccall unsafe "offset_StgTSO_what_next"
  offset_StgTSO_what_next :: Int

foreign import ccall unsafe "offset_StgTSO_why_blocked"
  offset_StgTSO_why_blocked :: Int

foreign import ccall unsafe "offset_StgTSO_flags" offset_StgTSO_flags :: Int

foreign import ccall unsafe "offset_StgTSO_block_info"
  offset_StgTSO_block_info :: Int

foreign import ccall unsafe "offset_StgTSO_id" offset_StgTSO_id :: Int

foreign import ccall unsafe "offset_StgTSO_saved_errno"
  offset_StgTSO_saved_errno :: Int

foreign import ccall unsafe "offset_StgTSO_dirty" offset_StgTSO_dirty :: Int

foreign import ccall unsafe "offset_StgTSO_bound" offset_StgTSO_bound :: Int

foreign import ccall unsafe "offset_StgTSO_cap" offset_StgTSO_cap :: Int

foreign import ccall unsafe "offset_StgTSO_trec" offset_StgTSO_trec :: Int

foreign import ccall unsafe "offset_StgTSO_blocked_exceptions"
  offset_StgTSO_blocked_exceptions :: Int

foreign import ccall unsafe "offset_StgTSO_bq" offset_StgTSO_bq :: Int

foreign import ccall unsafe "offset_StgTSO_alloc_limit"
  offset_StgTSO_alloc_limit :: Int

foreign import ccall unsafe "offset_StgTSO_tot_stack_size"
  offset_StgTSO_tot_stack_size :: Int

foreign import ccall unsafe "offset_StgUpdateFrame_updatee"
  offset_StgUpdateFrame_updatee :: Int

foreign import ccall unsafe "sizeof_StgWeak" sizeof_StgWeak :: Int

foreign import ccall unsafe "offset_StgWeak_cfinalizers"
  offset_StgWeak_cfinalizers :: Int

foreign import ccall unsafe "offset_StgWeak_key" offset_StgWeak_key :: Int

foreign import ccall unsafe "offset_StgWeak_value" offset_StgWeak_value :: Int

foreign import ccall unsafe "offset_StgWeak_finalizer"
  offset_StgWeak_finalizer :: Int

foreign import ccall unsafe "offset_StgWeak_link" offset_StgWeak_link :: Int

foreign import ccall unsafe "next_ThreadRunGHC" next_ThreadRunGHC :: Int

foreign import ccall unsafe "next_ThreadInterpret" next_ThreadInterpret :: Int

foreign import ccall unsafe "next_ThreadKilled" next_ThreadKilled :: Int

foreign import ccall unsafe "next_ThreadComplete" next_ThreadComplete :: Int

foreign import ccall unsafe "bf_EVACUATED" bf_EVACUATED :: Int

foreign import ccall unsafe "bf_LARGE" bf_LARGE :: Int

foreign import ccall unsafe "bf_PINNED" bf_PINNED :: Int

foreign import ccall unsafe "bf_MARKED" bf_MARKED :: Int

foreign import ccall unsafe "bf_EXEC" bf_EXEC :: Int

foreign import ccall unsafe "bf_FRAGMENTED" bf_FRAGMENTED :: Int

foreign import ccall unsafe "bf_KNOWN" bf_KNOWN :: Int

foreign import ccall unsafe "bf_SWEPT" bf_SWEPT :: Int

foreign import ccall unsafe "bf_COMPACT" bf_COMPACT :: Int

foreign import ccall unsafe "blocked_NotBlocked" blocked_NotBlocked :: Int

foreign import ccall unsafe "blocked_BlockedOnMVar" blocked_BlockedOnMVar :: Int

foreign import ccall unsafe "blocked_BlockedOnMVarRead"
  blocked_BlockedOnMVarRead :: Int

foreign import ccall unsafe "blocked_BlockedOnBlackHole"
  blocked_BlockedOnBlackHole :: Int

foreign import ccall unsafe "blocked_BlockedOnRead" blocked_BlockedOnRead :: Int

foreign import ccall unsafe "blocked_BlockedOnWrite"
  blocked_BlockedOnWrite :: Int

foreign import ccall unsafe "blocked_BlockedOnDelay"
  blocked_BlockedOnDelay :: Int

foreign import ccall unsafe "blocked_BlockedOnSTM" blocked_BlockedOnSTM :: Int

foreign import ccall unsafe "blocked_BlockedOnDoProc"
  blocked_BlockedOnDoProc :: Int

foreign import ccall unsafe "blocked_BlockedOnCCall"
  blocked_BlockedOnCCall :: Int

foreign import ccall unsafe "blocked_BlockedOnCCall_Interruptible"
  blocked_BlockedOnCCall_Interruptible :: Int

foreign import ccall unsafe "blocked_BlockedOnMsgThrowTo"
  blocked_BlockedOnMsgThrowTo :: Int

foreign import ccall unsafe "blocked_ThreadMigrating"
  blocked_ThreadMigrating :: Int

foreign import ccall unsafe "ret_HeapOverflow" ret_HeapOverflow :: Int

foreign import ccall unsafe "ret_StackOverflow" ret_StackOverflow :: Int

foreign import ccall unsafe "ret_ThreadYielding" ret_ThreadYielding :: Int

foreign import ccall unsafe "ret_ThreadBlocked" ret_ThreadBlocked :: Int

foreign import ccall unsafe "ret_ThreadFinished" ret_ThreadFinished :: Int

foreign import ccall unsafe "sched_SCHED_RUNNING" sched_SCHED_RUNNING :: Int

foreign import ccall unsafe "sched_SCHED_INTERRUPTING"
  sched_SCHED_INTERRUPTING :: Int

foreign import ccall unsafe "sched_SCHED_SHUTTING_DOWN"
  sched_SCHED_SHUTTING_DOWN :: Int

foreign import ccall unsafe "scheduler_NoStatus" scheduler_NoStatus :: Int

foreign import ccall unsafe "scheduler_Success" scheduler_Success :: Int

foreign import ccall unsafe "scheduler_Killed" scheduler_Killed :: Int

foreign import ccall unsafe "scheduler_Interrupted" scheduler_Interrupted :: Int

foreign import ccall unsafe "scheduler_HeapExhausted"
  scheduler_HeapExhausted :: Int

foreign import ccall unsafe "sizeof_bool" sizeof_bool :: Int

foreign import ccall unsafe "sizeof_int" sizeof_int :: Int

foreign import ccall unsafe "sizeof_SchedulerStatus"
  sizeof_SchedulerStatus :: Int

foreign import ccall unsafe "tso_LOCKED" tso_LOCKED :: Int

foreign import ccall unsafe "tso_BLOCKEX" tso_BLOCKEX :: Int

foreign import ccall unsafe "tso_INTERRUPTIBLE" tso_INTERRUPTIBLE :: Int

foreign import ccall unsafe "tso_STOPPED_ON_BREAKPOINT"
  tso_STOPPED_ON_BREAKPOINT :: Int

foreign import ccall unsafe "tso_MARKED" tso_MARKED :: Int

foreign import ccall unsafe "tso_SQUEEZED" tso_SQUEEZED :: Int

foreign import ccall unsafe "tso_ALLOC_LIMIT" tso_ALLOC_LIMIT :: Int

foreign import ccall unsafe "sizeof_StgStableName" sizeof_StgStableName :: Int

foreign import ccall unsafe "offset_StgStableName_header"
  offset_StgStableName_header :: Int

foreign import ccall unsafe "offset_StgStableName_sn"
  offset_StgStableName_sn :: Int

foreign import ccall unsafe "clock_monotonic" clock_monotonic :: Int

foreign import ccall unsafe "clock_realtime" clock_realtime :: Int
