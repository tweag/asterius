module Language.Haskell.GHC.Toolkit.Constants where

foreign import ccall unsafe "roundup_bytes_to_words" roundup_bytes_to_words
  :: Int -> Int

foreign import ccall unsafe "block_size" block_size :: Int

foreign import ccall unsafe "mblock_size" mblock_size :: Int

foreign import ccall unsafe "mblock_mask" mblock_mask :: Int

foreign import ccall unsafe "blocks_per_mblock" blocks_per_mblock :: Int

foreign import ccall unsafe "offset_first_bdescr" offset_first_bdescr :: Int

foreign import ccall unsafe "offset_last_bdescr" offset_last_bdescr :: Int

foreign import ccall unsafe "offset_first_block" offset_first_block :: Int

foreign import ccall unsafe "offset_last_block" offset_last_block :: Int

foreign import ccall unsafe "sizeof_bdescr" sizeof_bdescr :: Int

foreign import ccall unsafe "offset_bdescr_start" offset_bdescr_start :: Int

foreign import ccall unsafe "offset_bdescr_free" offset_bdescr_free :: Int

foreign import ccall unsafe "offset_bdescr_link" offset_bdescr_link :: Int

foreign import ccall unsafe "offset_bdescr_flags" offset_bdescr_flags :: Int

foreign import ccall unsafe "offset_bdescr_blocks" offset_bdescr_blocks :: Int

foreign import ccall unsafe "sizeof_Capability" sizeof_Capability :: Int

foreign import ccall unsafe "offset_Capability_r" offset_Capability_r :: Int

foreign import ccall unsafe "offset_Capability_no" offset_Capability_no :: Int

foreign import ccall unsafe "offset_Capability_running_task" offset_Capability_running_task
  :: Int

foreign import ccall unsafe "offset_Capability_run_queue_hd" offset_Capability_run_queue_hd
  :: Int

foreign import ccall unsafe "offset_Capability_run_queue_tl" offset_Capability_run_queue_tl
  :: Int

foreign import ccall unsafe "offset_Capability_n_run_queue" offset_Capability_n_run_queue
  :: Int

foreign import ccall unsafe "offset_Capability_weak_ptr_list_hd" offset_Capability_weak_ptr_list_hd
  :: Int

foreign import ccall unsafe "offset_Capability_weak_ptr_list_tl" offset_Capability_weak_ptr_list_tl
  :: Int

foreign import ccall unsafe "offset_Capability_context_switch" offset_Capability_context_switch
  :: Int

foreign import ccall unsafe "offset_Capability_interrupt" offset_Capability_interrupt
  :: Int

foreign import ccall unsafe "offset_Capability_total_allocated" offset_Capability_total_allocated
  :: Int

foreign import ccall unsafe "sizeof_nursery" sizeof_nursery :: Int

foreign import ccall unsafe "offset_nursery_blocks" offset_nursery_blocks :: Int

foreign import ccall unsafe "offset_nursery_n_blocks" offset_nursery_n_blocks
  :: Int

foreign import ccall unsafe "sizeof_StgInd" sizeof_StgInd :: Int

foreign import ccall unsafe "offset_StgInd_indirectee" offset_StgInd_indirectee
  :: Int

foreign import ccall unsafe "sizeof_StgIndStatic" sizeof_StgIndStatic :: Int

foreign import ccall unsafe "offset_StgIndStatic_indirectee" offset_StgIndStatic_indirectee
  :: Int

foreign import ccall unsafe "offset_StgIndStatic_static_link" offset_StgIndStatic_static_link
  :: Int

foreign import ccall unsafe "offset_StgIndStatic_saved_info" offset_StgIndStatic_saved_info
  :: Int

foreign import ccall unsafe "sizeof_StgRegTable" sizeof_StgRegTable :: Int

foreign import ccall unsafe "offset_StgRegTable_rSp" offset_StgRegTable_rSp
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rSpLim" offset_StgRegTable_rSpLim
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rHp" offset_StgRegTable_rHp
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rHpLim" offset_StgRegTable_rHpLim
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rNursery" offset_StgRegTable_rNursery
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentTSO" offset_StgRegTable_rCurrentTSO
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentNursery" offset_StgRegTable_rCurrentNursery
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentAlloc" offset_StgRegTable_rCurrentAlloc
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rRet" offset_StgRegTable_rRet
  :: Int

foreign import ccall unsafe "sizeof_StgStack" sizeof_StgStack :: Int

foreign import ccall unsafe "offset_StgStack_stack_size" offset_StgStack_stack_size
  :: Int

foreign import ccall unsafe "offset_StgStack_dirty" offset_StgStack_dirty :: Int

foreign import ccall unsafe "offset_StgStack_sp" offset_StgStack_sp :: Int

foreign import ccall unsafe "offset_StgStack_stack" offset_StgStack_stack :: Int

foreign import ccall unsafe "sizeof_StgStopFrame" sizeof_StgStopFrame :: Int

foreign import ccall unsafe "sizeof_StgTSO" sizeof_StgTSO :: Int

foreign import ccall unsafe "offset_StgTSO__link" offset_StgTSO__link :: Int

foreign import ccall unsafe "offset_StgTSO_stackobj" offset_StgTSO_stackobj
  :: Int

foreign import ccall unsafe "offset_StgTSO_what_next" offset_StgTSO_what_next
  :: Int

foreign import ccall unsafe "offset_StgTSO_why_blocked" offset_StgTSO_why_blocked
  :: Int

foreign import ccall unsafe "offset_StgTSO_flags" offset_StgTSO_flags :: Int

foreign import ccall unsafe "offset_StgTSO_block_info" offset_StgTSO_block_info
  :: Int

foreign import ccall unsafe "offset_StgTSO_id" offset_StgTSO_id :: Int

foreign import ccall unsafe "offset_StgTSO_saved_errno" offset_StgTSO_saved_errno
  :: Int

foreign import ccall unsafe "offset_StgTSO_dirty" offset_StgTSO_dirty :: Int

foreign import ccall unsafe "offset_StgTSO_bound" offset_StgTSO_bound :: Int

foreign import ccall unsafe "offset_StgTSO_cap" offset_StgTSO_cap :: Int

foreign import ccall unsafe "offset_StgTSO_trec" offset_StgTSO_trec :: Int

foreign import ccall unsafe "offset_StgTSO_blocked_exceptions" offset_StgTSO_blocked_exceptions
  :: Int

foreign import ccall unsafe "offset_StgTSO_bq" offset_StgTSO_bq :: Int

foreign import ccall unsafe "offset_StgTSO_alloc_limit" offset_StgTSO_alloc_limit
  :: Int

foreign import ccall unsafe "offset_StgTSO_tot_stack_size" offset_StgTSO_tot_stack_size
  :: Int

foreign import ccall unsafe "offset_StgTSO_StgStack" offset_StgTSO_StgStack
  :: Int

foreign import ccall unsafe "sizeof_StgTSOBlockInfo" sizeof_StgTSOBlockInfo
  :: Int

foreign import ccall unsafe "offset_StgTSOBlockInfo_closure" offset_StgTSOBlockInfo_closure
  :: Int

foreign import ccall unsafe "offset_StgTSOBlockInfo_prev" offset_StgTSOBlockInfo_prev
  :: Int

foreign import ccall unsafe "sizeof_Task" sizeof_Task :: Int

foreign import ccall unsafe "offset_Task_cap" offset_Task_cap :: Int

foreign import ccall unsafe "offset_Task_incall" offset_Task_incall :: Int

foreign import ccall unsafe "offset_Task_n_spare_incalls" offset_Task_n_spare_incalls
  :: Int

foreign import ccall unsafe "offset_Task_spare_incalls" offset_Task_spare_incalls
  :: Int

foreign import ccall unsafe "offset_Task_worker" offset_Task_worker :: Int

foreign import ccall unsafe "offset_Task_stopped" offset_Task_stopped :: Int

foreign import ccall unsafe "offset_Task_running_finalizers" offset_Task_running_finalizers
  :: Int

foreign import ccall unsafe "offset_Task_preferred_capability" offset_Task_preferred_capability
  :: Int

foreign import ccall unsafe "offset_Task_next" offset_Task_next :: Int

foreign import ccall unsafe "offset_Task_all_next" offset_Task_all_next :: Int

foreign import ccall unsafe "offset_Task_all_prev" offset_Task_all_prev :: Int

foreign import ccall unsafe "sizeof_InCall" sizeof_InCall :: Int

foreign import ccall unsafe "offset_InCall_tso" offset_InCall_tso :: Int

foreign import ccall unsafe "offset_InCall_suspended_tso" offset_InCall_suspended_tso
  :: Int

foreign import ccall unsafe "offset_InCall_suspended_cap" offset_InCall_suspended_cap
  :: Int

foreign import ccall unsafe "offset_InCall_rstat" offset_InCall_rstat :: Int

foreign import ccall unsafe "offset_InCall_ret" offset_InCall_ret :: Int

foreign import ccall unsafe "offset_InCall_task" offset_InCall_task :: Int

foreign import ccall unsafe "offset_InCall_prev_stack" offset_InCall_prev_stack
  :: Int

foreign import ccall unsafe "offset_InCall_prev" offset_InCall_prev :: Int

foreign import ccall unsafe "offset_InCall_next" offset_InCall_next :: Int

foreign import ccall unsafe "next_ThreadRunGHC" next_ThreadRunGHC :: Int

foreign import ccall unsafe "next_ThreadInterpret" next_ThreadInterpret :: Int

foreign import ccall unsafe "next_ThreadKilled" next_ThreadKilled :: Int

foreign import ccall unsafe "next_ThreadComplete" next_ThreadComplete :: Int

foreign import ccall unsafe "blocked_NotBlocked" blocked_NotBlocked :: Int

foreign import ccall unsafe "blocked_BlockedOnMVar" blocked_BlockedOnMVar :: Int

foreign import ccall unsafe "blocked_BlockedOnMVarRead" blocked_BlockedOnMVarRead
  :: Int

foreign import ccall unsafe "blocked_BlockedOnBlackHole" blocked_BlockedOnBlackHole
  :: Int

foreign import ccall unsafe "blocked_BlockedOnRead" blocked_BlockedOnRead :: Int

foreign import ccall unsafe "blocked_BlockedOnWrite" blocked_BlockedOnWrite
  :: Int

foreign import ccall unsafe "blocked_BlockedOnDelay" blocked_BlockedOnDelay
  :: Int

foreign import ccall unsafe "blocked_BlockedOnSTM" blocked_BlockedOnSTM :: Int

foreign import ccall unsafe "blocked_BlockedOnDoProc" blocked_BlockedOnDoProc
  :: Int

foreign import ccall unsafe "blocked_BlockedOnCCall" blocked_BlockedOnCCall
  :: Int

foreign import ccall unsafe "blocked_BlockedOnCCall_Interruptible" blocked_BlockedOnCCall_Interruptible
  :: Int

foreign import ccall unsafe "blocked_BlockedOnMsgThrowTo" blocked_BlockedOnMsgThrowTo
  :: Int

foreign import ccall unsafe "blocked_ThreadMigrating" blocked_ThreadMigrating
  :: Int

foreign import ccall unsafe "scheduler_NoStatus" scheduler_NoStatus :: Int

foreign import ccall unsafe "scheduler_Success" scheduler_Success :: Int

foreign import ccall unsafe "scheduler_Killed" scheduler_Killed :: Int

foreign import ccall unsafe "scheduler_Interrupted" scheduler_Interrupted :: Int

foreign import ccall unsafe "scheduler_HeapExhausted" scheduler_HeapExhausted
  :: Int

foreign import ccall unsafe "sizeof_bool" sizeof_bool :: Int

foreign import ccall unsafe "sizeof_SchedulerStatus" sizeof_SchedulerStatus
  :: Int
