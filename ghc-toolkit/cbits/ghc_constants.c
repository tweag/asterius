#include <Rts.h>
#include <Schedule.h>
#include <Capability.h>

HsInt offset_timespec_tv_sec() { return offsetof(struct timespec, tv_sec); }

HsInt offset_timespec_tv_nsec() { return offsetof(struct timespec, tv_nsec); }

HsInt roundup(HsInt x, HsInt n) { return (x + (n - 1)) & (~(n - 1)); }

HsInt roundup_bytes_to_words(HsInt n) { return ROUNDUP_BYTES_TO_WDS(n); }

HsInt block_size() { return BLOCK_SIZE; }

HsInt mblock_size() { return MBLOCK_SIZE; }

HsInt blocks_per_mblock() { return BLOCKS_PER_MBLOCK; }

HsInt offset_first_bdescr() { return (HsInt)FIRST_BDESCR(0); }

HsInt offset_last_bdescr() { return (HsInt)LAST_BDESCR(0); }

HsInt offset_first_block() { return (HsInt)FIRST_BLOCK(0); }

HsInt offset_last_block() { return (HsInt)LAST_BLOCK(0); }

HsInt sizeof_bdescr() { return sizeof(bdescr); }

HsInt offset_bdescr_start() { return offsetof(bdescr, start); }

HsInt offset_bdescr_free() { return offsetof(bdescr, free); }

HsInt offset_bdescr_link() { return offsetof(bdescr, link); }

HsInt offset_bdescr_node() { return offsetof(bdescr, node); }

HsInt offset_bdescr_flags() { return offsetof(bdescr, flags); }

HsInt offset_bdescr_blocks() { return offsetof(bdescr, blocks); }

HsInt sizeof_Capability() { return sizeof(Capability); }

HsInt offset_Capability_f() { return offsetof(Capability, f); }

HsInt offset_Capability_r() { return offsetof(Capability, r); }

HsInt offset_Capability_no() { return offsetof(Capability, no); }

HsInt offset_Capability_node() { return offsetof(Capability, node); }

HsInt offset_Capability_running_task() {
  return offsetof(Capability, running_task);
}

HsInt offset_Capability_in_haskell() {
  return offsetof(Capability, in_haskell);
}

HsInt offset_Capability_idle() { return offsetof(Capability, idle); }

HsInt offset_Capability_disabled() { return offsetof(Capability, disabled); }

HsInt offset_Capability_run_queue_hd() {
  return offsetof(Capability, run_queue_hd);
}

HsInt offset_Capability_run_queue_tl() {
  return offsetof(Capability, run_queue_tl);
}

HsInt offset_Capability_n_run_queue() {
  return offsetof(Capability, n_run_queue);
}
HsInt offset_Capability_suspended_ccalls() {
  return offsetof(Capability, suspended_ccalls);
}
HsInt offset_Capability_n_suspended_ccalls() {
  return offsetof(Capability, n_suspended_ccalls);
}

HsInt offset_Capability_mut_lists() { return offsetof(Capability, mut_lists); }

HsInt offset_Capability_saved_mut_lists() {
  return offsetof(Capability, saved_mut_lists);
}

HsInt offset_Capability_pinned_object_block() {
  return offsetof(Capability, pinned_object_block);
}
HsInt offset_Capability_pinned_object_blocks() {
  return offsetof(Capability, pinned_object_blocks);
}

HsInt offset_Capability_weak_ptr_list_hd() {
  return offsetof(Capability, weak_ptr_list_hd);
}

HsInt offset_Capability_weak_ptr_list_tl() {
  return offsetof(Capability, weak_ptr_list_tl);
}

HsInt offset_Capability_context_switch() {
  return offsetof(Capability, context_switch);
}

HsInt offset_Capability_interrupt() { return offsetof(Capability, interrupt); }

HsInt offset_Capability_total_allocated() {
  return offsetof(Capability, total_allocated);
}

HsInt offset_Capability_free_tvar_watch_queues() {
  return offsetof(Capability, free_tvar_watch_queues);
}

HsInt offset_Capability_free_trec_chunks() {
  return offsetof(Capability, free_trec_chunks);
}
HsInt offset_Capability_free_trec_headers() {
  return offsetof(Capability, free_trec_headers);
}

HsInt offset_Capability_transaction_tokens() {
  return offsetof(Capability, transaction_tokens);
}

HsInt sizeof_MessageBlackHole() {
  return sizeof(MessageBlackHole);
}

HsInt offset_MessageBlackHole_link() {
  return offsetof(MessageBlackHole, link);
}

HsInt offset_MessageBlackHole_tso() {
  return offsetof(MessageBlackHole, tso);
}

HsInt offset_MessageBlackHole_bh() {
  return offsetof(MessageBlackHole, bh);
}

HsInt sizeof_StgAP() { return sizeof(StgAP); }

HsInt offset_StgAP_arity() { return offsetof(StgAP, arity); }

HsInt offset_StgAP_n_args() { return offsetof(StgAP, n_args); }

HsInt offset_StgAP_fun() { return offsetof(StgAP, fun); }

HsInt offset_StgAP_payload() { return offsetof(StgAP, payload); }

HsInt sizeof_StgAP_STACK() { return sizeof(StgAP_STACK); }

HsInt offset_StgAP_STACK_size() { return offsetof(StgAP_STACK, size); }

HsInt offset_StgAP_STACK_fun() { return offsetof(StgAP_STACK, fun); }

HsInt offset_StgAP_STACK_payload() { return offsetof(StgAP_STACK, payload); }

HsInt sizeof_StgArrBytes() { return sizeof(StgArrBytes); }

HsInt offset_StgArrBytes_bytes() { return offsetof(StgArrBytes, bytes); }

HsInt offset_StgArrBytes_payload() { return offsetof(StgArrBytes, payload); }

HsInt sizeof_StgBlockingQueue() {
  return sizeof(StgBlockingQueue);
}

HsInt offset_StgBlockingQueue_link() {
  return offsetof(StgBlockingQueue, link);
}

HsInt offset_StgBlockingQueue_bh() {
  return offsetof(StgBlockingQueue, bh);
}

HsInt offset_StgBlockingQueue_owner() {
  return offsetof(StgBlockingQueue, owner);
}

HsInt offset_StgBlockingQueue_queue() {
  return offsetof(StgBlockingQueue, queue);
}

HsInt sizeof_StgClosure() { return sizeof(StgClosure); }

HsInt offset_StgClosure_payload() { return offsetof(StgClosure, payload); }

HsInt sizeof_StgInd() { return sizeof(StgInd); }

HsInt offset_StgInd_indirectee() { return offsetof(StgInd, indirectee); }

HsInt sizeof_StgIndStatic() { return sizeof(StgIndStatic); }

HsInt offset_StgIndStatic_indirectee() {
  return offsetof(StgIndStatic, indirectee);
}

HsInt offset_StgIndStatic_static_link() {
  return offsetof(StgIndStatic, static_link);
}

HsInt offset_StgIndStatic_saved_info() {
  return offsetof(StgIndStatic, saved_info);
}

HsInt offset_StgFunInfoExtraFwd_fun_type() {
  return offsetof(StgFunInfoExtraFwd, fun_type);
}

HsInt offset_StgFunInfoExtraFwd_srt() {
  return offsetof(StgFunInfoExtraFwd, srt);
}

HsInt offset_StgFunInfoExtraFwd_b() { return offsetof(StgFunInfoExtraFwd, b); }

HsInt offset_StgFunInfoTable_i() { return offsetof(StgFunInfoTable, i); }

HsInt offset_StgFunInfoTable_f() { return offsetof(StgFunInfoTable, f); }

HsInt sizeof_StgFunTable() { return sizeof(StgFunTable); }

HsInt offset_StgFunTable_stgEagerBlackholeInfo() {
  return offsetof(StgFunTable, stgEagerBlackholeInfo);
}

HsInt offset_StgFunTable_stgGCEnter1() {
  return offsetof(StgFunTable, stgGCEnter1);
}

HsInt offset_StgFunTable_stgGCFun() { return offsetof(StgFunTable, stgGCFun); }

HsInt offset_StgInfoTable_entry() { return offsetof(StgInfoTable, entry); }

HsInt offset_StgInfoTable_layout() { return offsetof(StgInfoTable, layout); }

HsInt offset_StgInfoTable_type() { return offsetof(StgInfoTable, type); }

HsInt offset_StgInfoTable_srt() { return offsetof(StgInfoTable, srt); }

HsInt offset_StgLargeBitmap_size() { return offsetof(StgLargeBitmap, size); }

HsInt offset_StgLargeBitmap_bitmap() {
  return offsetof(StgLargeBitmap, bitmap);
}
HsInt sizeof_StgMutArrPtrs() { return sizeof(StgMutArrPtrs); }

HsInt offset_StgMutArrPtrs_ptrs() { return offsetof(StgMutArrPtrs, ptrs); }

HsInt offset_StgMutArrPtrs_size() { return offsetof(StgMutArrPtrs, size); }

HsInt offset_StgMutArrPtrs_payload() {
  return offsetof(StgMutArrPtrs, payload);
}

HsInt offset_StgMVar_head() { return offsetof(StgMVar, head); }

HsInt offset_StgMVar_tail() { return offsetof(StgMVar, tail); }

HsInt offset_StgMVar_value() { return offsetof(StgMVar, value); }

HsInt sizeof_StgPAP() { return sizeof(StgPAP); }

HsInt offset_StgPAP_arity() { return offsetof(StgPAP, arity); }

HsInt offset_StgPAP_n_args() { return offsetof(StgPAP, n_args); }

HsInt offset_StgPAP_fun() { return offsetof(StgPAP, fun); }

HsInt offset_StgPAP_payload() { return offsetof(StgPAP, payload); }

HsInt sizeof_StgRetFun() { return sizeof(StgRetFun); }

HsInt offset_StgRetFun_size() { return offsetof(StgRetFun, size); }

HsInt offset_StgRetFun_fun() { return offsetof(StgRetFun, fun); }

HsInt offset_StgRetFun_payload() { return offsetof(StgRetFun, payload); }

HsInt offset_StgRetInfoTable_i() { return offsetof(StgRetInfoTable, i); }

HsInt offset_StgRetInfoTable_srt() { return offsetof(StgRetInfoTable, srt); }

HsInt sizeof_StgRegTable() { return sizeof(StgRegTable); }

HsInt offset_StgRegTable_rR1() { return offsetof(StgRegTable, rR1); }

HsInt offset_StgRegTable_rR2() { return offsetof(StgRegTable, rR2); }

HsInt offset_StgRegTable_rR3() { return offsetof(StgRegTable, rR3); }

HsInt offset_StgRegTable_rR4() { return offsetof(StgRegTable, rR4); }

HsInt offset_StgRegTable_rR5() { return offsetof(StgRegTable, rR5); }

HsInt offset_StgRegTable_rR6() { return offsetof(StgRegTable, rR6); }

HsInt offset_StgRegTable_rR7() { return offsetof(StgRegTable, rR7); }

HsInt offset_StgRegTable_rR8() { return offsetof(StgRegTable, rR8); }

HsInt offset_StgRegTable_rR9() { return offsetof(StgRegTable, rR9); }

HsInt offset_StgRegTable_rR10() { return offsetof(StgRegTable, rR10); }

HsInt offset_StgRegTable_rF1() { return offsetof(StgRegTable, rF1); }

HsInt offset_StgRegTable_rF2() { return offsetof(StgRegTable, rF2); }

HsInt offset_StgRegTable_rF3() { return offsetof(StgRegTable, rF3); }

HsInt offset_StgRegTable_rF4() { return offsetof(StgRegTable, rF4); }

HsInt offset_StgRegTable_rF5() { return offsetof(StgRegTable, rF5); }

HsInt offset_StgRegTable_rF6() { return offsetof(StgRegTable, rF6); }

HsInt offset_StgRegTable_rD1() { return offsetof(StgRegTable, rD1); }

HsInt offset_StgRegTable_rD2() { return offsetof(StgRegTable, rD2); }

HsInt offset_StgRegTable_rD3() { return offsetof(StgRegTable, rD3); }

HsInt offset_StgRegTable_rD4() { return offsetof(StgRegTable, rD4); }

HsInt offset_StgRegTable_rD5() { return offsetof(StgRegTable, rD5); }

HsInt offset_StgRegTable_rD6() { return offsetof(StgRegTable, rD6); }

HsInt offset_StgRegTable_rL1() { return offsetof(StgRegTable, rL1); }

HsInt offset_StgRegTable_rSp() { return offsetof(StgRegTable, rSp); }

HsInt offset_StgRegTable_rSpLim() { return offsetof(StgRegTable, rSpLim); }

HsInt offset_StgRegTable_rHp() { return offsetof(StgRegTable, rHp); }

HsInt offset_StgRegTable_rHpLim() { return offsetof(StgRegTable, rHpLim); }

HsInt offset_StgRegTable_rCCCS() { return offsetof(StgRegTable, rCCCS); }

HsInt offset_StgRegTable_rNursery() { return offsetof(StgRegTable, rNursery); }

HsInt offset_StgRegTable_rCurrentTSO() {
  return offsetof(StgRegTable, rCurrentTSO);
}

HsInt offset_StgRegTable_rCurrentNursery() {
  return offsetof(StgRegTable, rCurrentNursery);
}

HsInt offset_StgRegTable_rCurrentAlloc() {
  return offsetof(StgRegTable, rCurrentAlloc);
}

HsInt offset_StgRegTable_rHpAlloc() { return offsetof(StgRegTable, rHpAlloc); }

HsInt offset_StgRegTable_rRet() { return offsetof(StgRegTable, rRet); }

HsInt sizeof_StgSelector() { return sizeof(StgSelector); }

HsInt offset_StgSelector_selectee() { return offsetof(StgSelector, selectee); }

HsInt sizeof_StgSmallMutArrPtrs() { return sizeof(StgSmallMutArrPtrs); }

HsInt offset_StgSmallMutArrPtrs_ptrs() {
  return offsetof(StgSmallMutArrPtrs, ptrs);
}

HsInt offset_StgSmallMutArrPtrs_payload() {
  return offsetof(StgSmallMutArrPtrs, payload);
}

HsInt sizeof_StgStack() { return sizeof(StgStack); }

HsInt offset_StgStack_stack_size() { return offsetof(StgStack, stack_size); }

HsInt offset_StgStack_dirty() { return offsetof(StgStack, dirty); }

HsInt offset_StgStack_sp() { return offsetof(StgStack, sp); }

HsInt offset_StgStack_stack() {
  return 8 * ROUNDUP_BYTES_TO_WDS(sizeof(StgStack));
}

HsInt sizeof_StgStopFrame() { return sizeof(StgStopFrame); }

HsInt sizeof_StgThunk() { return sizeof(StgThunk); }

HsInt offset_StgThunk_payload() { return offsetof(StgThunk, payload); }

HsInt offset_StgThunkInfoTable_i() { return offsetof(StgThunkInfoTable, i); }

HsInt offset_StgThunkInfoTable_srt() {
  return offsetof(StgThunkInfoTable, srt);
}

HsInt sizeof_StgTSO() { return sizeof(StgTSO); }

HsInt offset_StgTSO__link() { return offsetof(StgTSO, _link); }

HsInt offset_StgTSO_stackobj() { return offsetof(StgTSO, stackobj); }

HsInt offset_StgTSO_what_next() { return offsetof(StgTSO, what_next); }

HsInt offset_StgTSO_why_blocked() { return offsetof(StgTSO, why_blocked); }

HsInt offset_StgTSO_flags() { return offsetof(StgTSO, flags); }

HsInt offset_StgTSO_block_info() { return offsetof(StgTSO, block_info); }

HsInt offset_StgTSO_id() { return offsetof(StgTSO, id); }

HsInt offset_StgTSO_saved_errno() { return offsetof(StgTSO, saved_errno); }

HsInt offset_StgTSO_dirty() { return offsetof(StgTSO, dirty); }

HsInt offset_StgTSO_bound() { return offsetof(StgTSO, bound); }

HsInt offset_StgTSO_cap() { return offsetof(StgTSO, cap); }

HsInt offset_StgTSO_trec() { return offsetof(StgTSO, trec); }

HsInt offset_StgTSO_blocked_exceptions() {
  return offsetof(StgTSO, blocked_exceptions);
}

HsInt offset_StgTSO_bq() { return offsetof(StgTSO, bq); }

HsInt offset_StgTSO_alloc_limit() { return offsetof(StgTSO, alloc_limit); }

HsInt offset_StgTSO_tot_stack_size() {
  return offsetof(StgTSO, tot_stack_size);
}

HsInt offset_StgUpdateFrame_updatee() {
  return offsetof(StgUpdateFrame, updatee);
}

HsInt sizeof_StgWeak() { return sizeof(StgWeak); }

HsInt offset_StgWeak_cfinalizers() { return offsetof(StgWeak, cfinalizers); }

HsInt offset_StgWeak_key() { return offsetof(StgWeak, key); }

HsInt offset_StgWeak_value() { return offsetof(StgWeak, value); }

HsInt offset_StgWeak_finalizer() { return offsetof(StgWeak, finalizer); }

HsInt offset_StgWeak_link() { return offsetof(StgWeak, link); }

HsInt next_ThreadRunGHC() { return ThreadRunGHC; }

HsInt next_ThreadInterpret() { return ThreadInterpret; }

HsInt next_ThreadKilled() { return ThreadKilled; }

HsInt next_ThreadComplete() { return ThreadComplete; }

HsInt bf_EVACUATED() { return BF_EVACUATED; }

HsInt bf_LARGE() { return BF_LARGE; }

HsInt bf_PINNED() { return BF_PINNED; }

HsInt bf_MARKED() { return BF_MARKED; }

HsInt bf_EXEC() { return BF_EXEC; }

HsInt bf_FRAGMENTED() { return BF_FRAGMENTED; }

HsInt bf_KNOWN() { return BF_KNOWN; }

HsInt bf_SWEPT() { return BF_SWEPT; }

HsInt bf_COMPACT() { return BF_COMPACT; }

HsInt blocked_NotBlocked() { return NotBlocked; }

HsInt blocked_BlockedOnMVar() { return BlockedOnMVar; }

HsInt blocked_BlockedOnMVarRead() { return BlockedOnMVarRead; }

HsInt blocked_BlockedOnBlackHole() { return BlockedOnBlackHole; }

HsInt blocked_BlockedOnRead() { return BlockedOnRead; }

HsInt blocked_BlockedOnWrite() { return BlockedOnWrite; }

HsInt blocked_BlockedOnDelay() { return BlockedOnDelay; }

HsInt blocked_BlockedOnSTM() { return BlockedOnSTM; }

HsInt blocked_BlockedOnDoProc() { return BlockedOnDoProc; }

HsInt blocked_BlockedOnCCall() { return BlockedOnCCall; }

HsInt blocked_BlockedOnCCall_Interruptible() {
  return BlockedOnCCall_Interruptible;
}

HsInt blocked_BlockedOnMsgThrowTo() { return BlockedOnMsgThrowTo; }

HsInt blocked_ThreadMigrating() { return ThreadMigrating; }

HsInt ret_HeapOverflow() { return HeapOverflow; }

HsInt ret_StackOverflow() { return StackOverflow; }

HsInt ret_ThreadYielding() { return ThreadYielding; }

HsInt ret_ThreadBlocked() { return ThreadBlocked; }

HsInt ret_ThreadFinished() { return ThreadFinished; }

HsInt sched_SCHED_RUNNING() { return SCHED_RUNNING; }

HsInt sched_SCHED_INTERRUPTING() { return SCHED_INTERRUPTING; }

HsInt sched_SCHED_SHUTTING_DOWN() { return SCHED_SHUTTING_DOWN; }

HsInt scheduler_NoStatus() { return NoStatus; }

HsInt scheduler_Success() { return Success; }

HsInt scheduler_Killed() { return Killed; }

HsInt scheduler_Interrupted() { return Interrupted; }

HsInt scheduler_HeapExhausted() { return HeapExhausted; }

HsInt sizeof_bool() { return sizeof(bool); }

HsInt sizeof_int() { return sizeof(int); }

HsInt sizeof_SchedulerStatus() { return sizeof(SchedulerStatus); }

HsInt tso_LOCKED() { return TSO_LOCKED; }

HsInt tso_BLOCKEX() { return TSO_BLOCKEX; }

HsInt tso_INTERRUPTIBLE() { return TSO_INTERRUPTIBLE; }

HsInt tso_STOPPED_ON_BREAKPOINT() { return TSO_STOPPED_ON_BREAKPOINT; }

HsInt tso_MARKED() { return TSO_MARKED; }

HsInt tso_SQUEEZED() { return TSO_SQUEEZED; }

HsInt tso_ALLOC_LIMIT() { return TSO_ALLOC_LIMIT; }

HsInt sizeof_StgStableName() { return sizeof(StgStableName); }

HsInt offset_StgStableName_header() { return offsetof(StgStableName, header); }

HsInt offset_StgStableName_sn() { return offsetof(StgStableName, sn); }

HsInt clock_monotonic() { return CLOCK_MONOTONIC; }

HsInt clock_realtime() { return CLOCK_REALTIME; }
