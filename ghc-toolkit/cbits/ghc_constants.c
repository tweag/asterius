#include <Rts.h>
#include <Capability.h>

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

HsInt offset_Capability_r() { return offsetof(Capability, r); }

HsInt offset_Capability_no() { return offsetof(Capability, no); }

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

HsInt sizeof_StgInd() { return sizeof(StgInd); }

HsInt offset_StgInd_indirectee() { return offsetof(StgInd, indirectee); }

HsInt sizeof_StgRegTable() { return sizeof(StgRegTable); }

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
HsInt offset_StgRegTable_rRet() { return offsetof(StgRegTable, rRet); }

HsInt sizeof_StgStack() { return sizeof(StgStack); }

HsInt offset_StgStack_stack_size() { return offsetof(StgStack, stack_size); }

HsInt offset_StgStack_dirty() { return offsetof(StgStack, dirty); }

HsInt offset_StgStack_sp() { return offsetof(StgStack, sp); }

HsInt offset_StgStack_stack() {
  return ROUNDUP_BYTES_TO_WDS(offsetof(StgStack, stack));
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

HsInt offset_StgTSO_StgStack() {
  return ROUNDUP_BYTES_TO_WDS(sizeof(StgTSO)) * 8;
}

HsInt sizeof_StgTSOBlockInfo() { return sizeof(StgTSOBlockInfo); }

HsInt offset_StgTSOBlockInfo_closure() {
  return offsetof(StgTSOBlockInfo, closure);
}

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
