#include <Rts.h>
#include <Capability.h>

HsInt roundup(HsInt x, HsInt n) { return (x + (n - 1)) & (~(n - 1)); }

HsInt roundup_bytes_to_words(HsInt n) { return ROUNDUP_BYTES_TO_WDS(n); }

HsInt block_size() { return BLOCK_SIZE; }

HsInt mblock_size() { return MBLOCK_SIZE; }

HsInt mblock_mask() { return MBLOCK_MASK; }

HsInt blocks_per_mblock() { return BLOCKS_PER_MBLOCK; }

HsInt offset_first_bdescr() { return (HsInt)FIRST_BDESCR(0); }

HsInt offset_last_bdescr() { return (HsInt)LAST_BDESCR(0); }

HsInt offset_first_block() { return (HsInt)FIRST_BLOCK(0); }

HsInt offset_last_block() { return (HsInt)LAST_BLOCK(0); }

HsInt sizeof_bdescr() { return sizeof(bdescr); }

HsInt offset_bdescr_start() { return offsetof(bdescr, start); }

HsInt offset_bdescr_free() { return offsetof(bdescr, free); }

HsInt offset_bdescr_link() { return offsetof(bdescr, link); }

HsInt offset_bdescr_flags() { return offsetof(bdescr, flags); }

HsInt offset_bdescr_blocks() { return offsetof(bdescr, blocks); }

HsInt sizeof_Capability() { return sizeof(Capability); }

HsInt offset_Capability_f() { return offsetof(Capability, f); }

HsInt offset_Capability_r() { return offsetof(Capability, r); }

HsInt offset_Capability_no() { return offsetof(Capability, no); }

HsInt offset_Capability_running_task() {
  return offsetof(Capability, running_task);
}

HsInt offset_Capability_in_haskell() {
  return offsetof(Capability, in_haskell);
}

HsInt offset_Capability_idle() { return offsetof(Capability, idle); }

HsInt offset_Capability_run_queue_hd() {
  return offsetof(Capability, run_queue_hd);
}

HsInt offset_Capability_run_queue_tl() {
  return offsetof(Capability, run_queue_tl);
}

HsInt offset_Capability_n_run_queue() {
  return offsetof(Capability, n_run_queue);
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

HsInt sizeof_nursery() { return sizeof(nursery); }

HsInt offset_nursery_blocks() { return offsetof(nursery, blocks); }

HsInt offset_nursery_n_blocks() { return offsetof(nursery, n_blocks); }

HsInt sizeof_RTS_FLAGS() { return sizeof(RTS_FLAGS); }

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

HsInt sizeof_StgFunTable() { return sizeof(StgFunTable); }

HsInt offset_StgFunTable_stgEagerBlackholeInfo() {
  return offsetof(StgFunTable, stgEagerBlackholeInfo);
}

HsInt offset_StgFunTable_stgGCEnter1() {
  return offsetof(StgFunTable, stgGCEnter1);
}

HsInt offset_StgFunTable_stgGCFun() { return offsetof(StgFunTable, stgGCFun); }

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

HsInt sizeof_StgStack() { return sizeof(StgStack); }

HsInt offset_StgStack_stack_size() { return offsetof(StgStack, stack_size); }

HsInt offset_StgStack_dirty() { return offsetof(StgStack, dirty); }

HsInt offset_StgStack_sp() { return offsetof(StgStack, sp); }

HsInt offset_StgStack_stack() {
  return 8 * ROUNDUP_BYTES_TO_WDS(sizeof(StgStack));
}

HsInt sizeof_StgStopFrame() { return sizeof(StgStopFrame); }

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

HsInt sizeof_StgTSOBlockInfo() { return sizeof(StgTSOBlockInfo); }

HsInt offset_StgTSOBlockInfo_closure() {
  return offsetof(StgTSOBlockInfo, closure);
}

HsInt offset_StgTSOBlockInfo_prev() { return offsetof(StgTSOBlockInfo, prev); }

HsInt sizeof_Task() { return sizeof(Task); }

HsInt offset_Task_cap() { return offsetof(Task, cap); }

HsInt offset_Task_incall() { return offsetof(Task, incall); }

HsInt offset_Task_n_spare_incalls() { return offsetof(Task, n_spare_incalls); }

HsInt offset_Task_spare_incalls() { return offsetof(Task, spare_incalls); }

HsInt offset_Task_worker() { return offsetof(Task, worker); }

HsInt offset_Task_stopped() { return offsetof(Task, stopped); }

HsInt offset_Task_running_finalizers() {
  return offsetof(Task, running_finalizers);
}

HsInt offset_Task_preferred_capability() {
  return offsetof(Task, preferred_capability);
}

HsInt offset_Task_next() { return offsetof(Task, next); }

HsInt offset_Task_all_next() { return offsetof(Task, all_next); }

HsInt offset_Task_all_prev() { return offsetof(Task, all_prev); }

HsInt sizeof_InCall() { return sizeof(InCall); }

HsInt offset_InCall_tso() { return offsetof(InCall, tso); }

HsInt offset_InCall_suspended_tso() { return offsetof(InCall, suspended_tso); }

HsInt offset_InCall_suspended_cap() { return offsetof(InCall, suspended_cap); }

HsInt offset_InCall_rstat() { return offsetof(InCall, rstat); }

HsInt offset_InCall_ret() { return offsetof(InCall, ret); }

HsInt offset_InCall_task() { return offsetof(InCall, task); }

HsInt offset_InCall_prev_stack() { return offsetof(InCall, prev_stack); }

HsInt offset_InCall_prev() { return offsetof(InCall, prev); }

HsInt offset_InCall_next() { return offsetof(InCall, next); }

HsInt next_ThreadRunGHC() { return ThreadRunGHC; }

HsInt next_ThreadInterpret() { return ThreadInterpret; }

HsInt next_ThreadKilled() { return ThreadKilled; }

HsInt next_ThreadComplete() { return ThreadComplete; }

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

HsInt scheduler_NoStatus() { return NoStatus; }

HsInt scheduler_Success() { return Success; }

HsInt scheduler_Killed() { return Killed; }

HsInt scheduler_Interrupted() { return Interrupted; }

HsInt scheduler_HeapExhausted() { return HeapExhausted; }

HsInt sizeof_bool() { return sizeof(bool); }

HsInt sizeof_int() { return sizeof(int); }

HsInt sizeof_SchedulerStatus() { return sizeof(SchedulerStatus); }
