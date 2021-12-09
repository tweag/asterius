#include "Rts.h"
#include "Schedule.h"
#include "Capability.h"

__attribute__((export_name("offset_timespec_tv_sec"))) uint32_t offset_timespec_tv_sec() { return offsetof(struct timespec, tv_sec); }

__attribute__((export_name("offset_timespec_tv_nsec"))) uint32_t offset_timespec_tv_nsec() { return offsetof(struct timespec, tv_nsec); }

__attribute__((export_name("roundup"))) uint32_t roundup(uint32_t x, uint32_t n) { return (x + (n - 1)) & (~(n - 1)); }

__attribute__((export_name("roundup_bytes_to_words"))) uint32_t roundup_bytes_to_words(uint32_t n) { return ROUNDUP_BYTES_TO_WDS(n); }

__attribute__((export_name("block_size"))) uint32_t block_size() { return BLOCK_SIZE; }

__attribute__((export_name("mblock_size"))) uint32_t mblock_size() { return MBLOCK_SIZE; }

__attribute__((export_name("offset_bdescr_start"))) uint32_t offset_bdescr_start() { return offsetof(bdescr, start); }

__attribute__((export_name("offset_bdescr_free"))) uint32_t offset_bdescr_free() { return offsetof(bdescr, free); }

__attribute__((export_name("offset_bdescr_gen_no"))) uint32_t offset_bdescr_gen_no() { return offsetof(bdescr, gen_no); }

__attribute__((export_name("offset_bdescr_flags"))) uint32_t offset_bdescr_flags() { return offsetof(bdescr, flags); }

__attribute__((export_name("offset_bdescr_blocks"))) uint32_t offset_bdescr_blocks() { return offsetof(bdescr, blocks); }

__attribute__((export_name("sizeof_Capability"))) uint32_t sizeof_Capability() { return sizeof(Capability); }

__attribute__((export_name("offset_Capability_f"))) uint32_t offset_Capability_f() { return offsetof(Capability, f); }

__attribute__((export_name("offset_Capability_r"))) uint32_t offset_Capability_r() { return offsetof(Capability, r); }

__attribute__((export_name("offset_Capability_no"))) uint32_t offset_Capability_no() { return offsetof(Capability, no); }

__attribute__((export_name("offset_Capability_node"))) uint32_t offset_Capability_node() { return offsetof(Capability, node); }

__attribute__((export_name("offset_Capability_idle"))) uint32_t offset_Capability_idle() { return offsetof(Capability, idle); }

__attribute__((export_name("offset_Capability_disabled"))) uint32_t offset_Capability_disabled() { return offsetof(Capability, disabled); }

__attribute__((export_name("offset_Capability_weak_ptr_list_hd"))) uint32_t offset_Capability_weak_ptr_list_hd() {
return offsetof(Capability, weak_ptr_list_hd);
}

__attribute__((export_name("offset_Capability_weak_ptr_list_tl"))) uint32_t offset_Capability_weak_ptr_list_tl() {
return offsetof(Capability, weak_ptr_list_tl);
}

__attribute__((export_name("offset_Capability_context_switch"))) uint32_t offset_Capability_context_switch() {
return offsetof(Capability, context_switch);
}

__attribute__((export_name("offset_Capability_interrupt"))) uint32_t offset_Capability_interrupt() { return offsetof(Capability, interrupt); }

__attribute__((export_name("offset_Capability_total_allocated"))) uint32_t offset_Capability_total_allocated() {
return offsetof(Capability, total_allocated);
}

__attribute__((export_name("offset_MessageBlackHole_link"))) uint32_t offset_MessageBlackHole_link() {
return offsetof(MessageBlackHole, link);
}

__attribute__((export_name("offset_MessageBlackHole_tso"))) uint32_t offset_MessageBlackHole_tso() {
return offsetof(MessageBlackHole, tso);
}

__attribute__((export_name("offset_MessageBlackHole_bh"))) uint32_t offset_MessageBlackHole_bh() {
return offsetof(MessageBlackHole, bh);
}

__attribute__((export_name("sizeof_StgAP"))) uint32_t sizeof_StgAP() { return sizeof(StgAP); }

__attribute__((export_name("offset_StgAP_arity"))) uint32_t offset_StgAP_arity() { return offsetof(StgAP, arity); }

__attribute__((export_name("offset_StgAP_n_args"))) uint32_t offset_StgAP_n_args() { return offsetof(StgAP, n_args); }

__attribute__((export_name("offset_StgAP_fun"))) uint32_t offset_StgAP_fun() { return offsetof(StgAP, fun); }

__attribute__((export_name("offset_StgAP_payload"))) uint32_t offset_StgAP_payload() { return offsetof(StgAP, payload); }

__attribute__((export_name("sizeof_StgAP_STACK"))) uint32_t sizeof_StgAP_STACK() { return sizeof(StgAP_STACK); }

__attribute__((export_name("offset_StgAP_STACK_size"))) uint32_t offset_StgAP_STACK_size() { return offsetof(StgAP_STACK, size); }

__attribute__((export_name("offset_StgAP_STACK_fun"))) uint32_t offset_StgAP_STACK_fun() { return offsetof(StgAP_STACK, fun); }

__attribute__((export_name("offset_StgAP_STACK_payload"))) uint32_t offset_StgAP_STACK_payload() { return offsetof(StgAP_STACK, payload); }

__attribute__((export_name("sizeof_StgArrBytes"))) uint32_t sizeof_StgArrBytes() { return sizeof(StgArrBytes); }

__attribute__((export_name("offset_StgArrBytes_bytes"))) uint32_t offset_StgArrBytes_bytes() { return offsetof(StgArrBytes, bytes); }

__attribute__((export_name("sizeof_StgBlockingQueue"))) uint32_t sizeof_StgBlockingQueue() {
return sizeof(StgBlockingQueue);
}

__attribute__((export_name("offset_StgBlockingQueue_link"))) uint32_t offset_StgBlockingQueue_link() {
return offsetof(StgBlockingQueue, link);
}

__attribute__((export_name("offset_StgBlockingQueue_bh"))) uint32_t offset_StgBlockingQueue_bh() {
return offsetof(StgBlockingQueue, bh);
}

__attribute__((export_name("offset_StgBlockingQueue_owner"))) uint32_t offset_StgBlockingQueue_owner() {
return offsetof(StgBlockingQueue, owner);
}

__attribute__((export_name("offset_StgBlockingQueue_queue"))) uint32_t offset_StgBlockingQueue_queue() {
return offsetof(StgBlockingQueue, queue);
}

__attribute__((export_name("offset_StgClosure_payload"))) uint32_t offset_StgClosure_payload() { return offsetof(StgClosure, payload); }

__attribute__((export_name("sizeof_StgInd"))) uint32_t sizeof_StgInd() { return sizeof(StgInd); }

__attribute__((export_name("offset_StgInd_indirectee"))) uint32_t offset_StgInd_indirectee() { return offsetof(StgInd, indirectee); }

__attribute__((export_name("sizeof_StgIndStatic"))) uint32_t sizeof_StgIndStatic() { return sizeof(StgIndStatic); }

__attribute__((export_name("offset_StgIndStatic_indirectee"))) uint32_t offset_StgIndStatic_indirectee() {
return offsetof(StgIndStatic, indirectee);
}

__attribute__((export_name("offset_StgIndStatic_saved_info"))) uint32_t offset_StgIndStatic_saved_info() {
return offsetof(StgIndStatic, saved_info);
}

__attribute__((export_name("offset_StgFunInfoExtraFwd_fun_type"))) uint32_t offset_StgFunInfoExtraFwd_fun_type() {
return offsetof(StgFunInfoExtraFwd, fun_type);
}

__attribute__((export_name("offset_StgFunInfoExtraFwd_srt"))) uint32_t offset_StgFunInfoExtraFwd_srt() {
return offsetof(StgFunInfoExtraFwd, srt);
}

__attribute__((export_name("offset_StgFunInfoExtraFwd_b"))) uint32_t offset_StgFunInfoExtraFwd_b() { return offsetof(StgFunInfoExtraFwd, b); }

__attribute__((export_name("offset_StgFunInfoTable_i"))) uint32_t offset_StgFunInfoTable_i() { return offsetof(StgFunInfoTable, i); }

__attribute__((export_name("offset_StgFunInfoTable_f"))) uint32_t offset_StgFunInfoTable_f() { return offsetof(StgFunInfoTable, f); }

__attribute__((export_name("offset_StgFunTable_stgEagerBlackholeInfo"))) uint32_t offset_StgFunTable_stgEagerBlackholeInfo() {
return offsetof(StgFunTable, stgEagerBlackholeInfo);
}

__attribute__((export_name("offset_StgFunTable_stgGCEnter1"))) uint32_t offset_StgFunTable_stgGCEnter1() {
return offsetof(StgFunTable, stgGCEnter1);
}

__attribute__((export_name("offset_StgFunTable_stgGCFun"))) uint32_t offset_StgFunTable_stgGCFun() { return offsetof(StgFunTable, stgGCFun); }

__attribute__((export_name("offset_StgInfoTable_layout"))) uint32_t offset_StgInfoTable_layout() { return offsetof(StgInfoTable, layout); }

__attribute__((export_name("offset_StgInfoTable_type"))) uint32_t offset_StgInfoTable_type() { return offsetof(StgInfoTable, type); }

__attribute__((export_name("offset_StgInfoTable_srt"))) uint32_t offset_StgInfoTable_srt() { return offsetof(StgInfoTable, srt); }

__attribute__((export_name("offset_StgLargeBitmap_size"))) uint32_t offset_StgLargeBitmap_size() { return offsetof(StgLargeBitmap, size); }

__attribute__((export_name("offset_StgLargeBitmap_bitmap"))) uint32_t offset_StgLargeBitmap_bitmap() {
return offsetof(StgLargeBitmap, bitmap);
}

__attribute__((export_name("sizeof_StgMutArrPtrs"))) uint32_t sizeof_StgMutArrPtrs() { return sizeof(StgMutArrPtrs); }

__attribute__((export_name("offset_StgMutArrPtrs_ptrs"))) uint32_t offset_StgMutArrPtrs_ptrs() { return offsetof(StgMutArrPtrs, ptrs); }

__attribute__((export_name("offset_StgMutArrPtrs_payload"))) uint32_t offset_StgMutArrPtrs_payload() {
return offsetof(StgMutArrPtrs, payload);
}

__attribute__((export_name("offset_StgMVar_head"))) uint32_t offset_StgMVar_head() { return offsetof(StgMVar, head); }

__attribute__((export_name("offset_StgMVar_tail"))) uint32_t offset_StgMVar_tail() { return offsetof(StgMVar, tail); }

__attribute__((export_name("offset_StgMVar_value"))) uint32_t offset_StgMVar_value() { return offsetof(StgMVar, value); }

__attribute__((export_name("sizeof_StgPAP"))) uint32_t sizeof_StgPAP() { return sizeof(StgPAP); }

__attribute__((export_name("offset_StgPAP_arity"))) uint32_t offset_StgPAP_arity() { return offsetof(StgPAP, arity); }

__attribute__((export_name("offset_StgPAP_n_args"))) uint32_t offset_StgPAP_n_args() { return offsetof(StgPAP, n_args); }

__attribute__((export_name("offset_StgPAP_fun"))) uint32_t offset_StgPAP_fun() { return offsetof(StgPAP, fun); }

__attribute__((export_name("offset_StgPAP_payload"))) uint32_t offset_StgPAP_payload() { return offsetof(StgPAP, payload); }

__attribute__((export_name("sizeof_StgRetFun"))) uint32_t sizeof_StgRetFun() { return sizeof(StgRetFun); }

__attribute__((export_name("offset_StgRetFun_size"))) uint32_t offset_StgRetFun_size() { return offsetof(StgRetFun, size); }

__attribute__((export_name("offset_StgRetFun_fun"))) uint32_t offset_StgRetFun_fun() { return offsetof(StgRetFun, fun); }

__attribute__((export_name("offset_StgRetFun_payload"))) uint32_t offset_StgRetFun_payload() { return offsetof(StgRetFun, payload); }

__attribute__((export_name("offset_StgRetInfoTable_i"))) uint32_t offset_StgRetInfoTable_i() { return offsetof(StgRetInfoTable, i); }

__attribute__((export_name("offset_StgRetInfoTable_srt"))) uint32_t offset_StgRetInfoTable_srt() { return offsetof(StgRetInfoTable, srt); }

__attribute__((export_name("offset_StgRegTable_rR1"))) uint32_t offset_StgRegTable_rR1() { return offsetof(StgRegTable, rR1); }

__attribute__((export_name("offset_StgRegTable_rR2"))) uint32_t offset_StgRegTable_rR2() { return offsetof(StgRegTable, rR2); }

__attribute__((export_name("offset_StgRegTable_rR3"))) uint32_t offset_StgRegTable_rR3() { return offsetof(StgRegTable, rR3); }

__attribute__((export_name("offset_StgRegTable_rR4"))) uint32_t offset_StgRegTable_rR4() { return offsetof(StgRegTable, rR4); }

__attribute__((export_name("offset_StgRegTable_rR5"))) uint32_t offset_StgRegTable_rR5() { return offsetof(StgRegTable, rR5); }

__attribute__((export_name("offset_StgRegTable_rR6"))) uint32_t offset_StgRegTable_rR6() { return offsetof(StgRegTable, rR6); }

__attribute__((export_name("offset_StgRegTable_rR7"))) uint32_t offset_StgRegTable_rR7() { return offsetof(StgRegTable, rR7); }

__attribute__((export_name("offset_StgRegTable_rR8"))) uint32_t offset_StgRegTable_rR8() { return offsetof(StgRegTable, rR8); }

__attribute__((export_name("offset_StgRegTable_rR9"))) uint32_t offset_StgRegTable_rR9() { return offsetof(StgRegTable, rR9); }

__attribute__((export_name("offset_StgRegTable_rR10"))) uint32_t offset_StgRegTable_rR10() { return offsetof(StgRegTable, rR10); }

__attribute__((export_name("offset_StgRegTable_rF1"))) uint32_t offset_StgRegTable_rF1() { return offsetof(StgRegTable, rF1); }

__attribute__((export_name("offset_StgRegTable_rF2"))) uint32_t offset_StgRegTable_rF2() { return offsetof(StgRegTable, rF2); }

__attribute__((export_name("offset_StgRegTable_rF3"))) uint32_t offset_StgRegTable_rF3() { return offsetof(StgRegTable, rF3); }

__attribute__((export_name("offset_StgRegTable_rF4"))) uint32_t offset_StgRegTable_rF4() { return offsetof(StgRegTable, rF4); }

__attribute__((export_name("offset_StgRegTable_rF5"))) uint32_t offset_StgRegTable_rF5() { return offsetof(StgRegTable, rF5); }

__attribute__((export_name("offset_StgRegTable_rF6"))) uint32_t offset_StgRegTable_rF6() { return offsetof(StgRegTable, rF6); }

__attribute__((export_name("offset_StgRegTable_rD1"))) uint32_t offset_StgRegTable_rD1() { return offsetof(StgRegTable, rD1); }

__attribute__((export_name("offset_StgRegTable_rD2"))) uint32_t offset_StgRegTable_rD2() { return offsetof(StgRegTable, rD2); }

__attribute__((export_name("offset_StgRegTable_rD3"))) uint32_t offset_StgRegTable_rD3() { return offsetof(StgRegTable, rD3); }

__attribute__((export_name("offset_StgRegTable_rD4"))) uint32_t offset_StgRegTable_rD4() { return offsetof(StgRegTable, rD4); }

__attribute__((export_name("offset_StgRegTable_rD5"))) uint32_t offset_StgRegTable_rD5() { return offsetof(StgRegTable, rD5); }

__attribute__((export_name("offset_StgRegTable_rD6"))) uint32_t offset_StgRegTable_rD6() { return offsetof(StgRegTable, rD6); }

__attribute__((export_name("offset_StgRegTable_rL1"))) uint32_t offset_StgRegTable_rL1() { return offsetof(StgRegTable, rL1); }

__attribute__((export_name("offset_StgRegTable_rSp"))) uint32_t offset_StgRegTable_rSp() { return offsetof(StgRegTable, rSp); }

__attribute__((export_name("offset_StgRegTable_rSpLim"))) uint32_t offset_StgRegTable_rSpLim() { return offsetof(StgRegTable, rSpLim); }

__attribute__((export_name("offset_StgRegTable_rHp"))) uint32_t offset_StgRegTable_rHp() { return offsetof(StgRegTable, rHp); }

__attribute__((export_name("offset_StgRegTable_rHpLim"))) uint32_t offset_StgRegTable_rHpLim() { return offsetof(StgRegTable, rHpLim); }

__attribute__((export_name("offset_StgRegTable_rCCCS"))) uint32_t offset_StgRegTable_rCCCS() { return offsetof(StgRegTable, rCCCS); }

__attribute__((export_name("offset_StgRegTable_rCurrentTSO"))) uint32_t offset_StgRegTable_rCurrentTSO() {
return offsetof(StgRegTable, rCurrentTSO);
}

__attribute__((export_name("offset_StgRegTable_rCurrentNursery"))) uint32_t offset_StgRegTable_rCurrentNursery() {
return offsetof(StgRegTable, rCurrentNursery);
}

__attribute__((export_name("offset_StgRegTable_rHpAlloc"))) uint32_t offset_StgRegTable_rHpAlloc() { return offsetof(StgRegTable, rHpAlloc); }

__attribute__((export_name("offset_StgRegTable_rRet"))) uint32_t offset_StgRegTable_rRet() { return offsetof(StgRegTable, rRet); }

__attribute__((export_name("sizeof_StgSelector"))) uint32_t sizeof_StgSelector() { return sizeof(StgSelector); }

__attribute__((export_name("offset_StgSelector_selectee"))) uint32_t offset_StgSelector_selectee() { return offsetof(StgSelector, selectee); }

__attribute__((export_name("sizeof_StgSmallMutArrPtrs"))) uint32_t sizeof_StgSmallMutArrPtrs() { return sizeof(StgSmallMutArrPtrs); }

__attribute__((export_name("offset_StgSmallMutArrPtrs_ptrs"))) uint32_t offset_StgSmallMutArrPtrs_ptrs() {
return offsetof(StgSmallMutArrPtrs, ptrs);
}

__attribute__((export_name("offset_StgSmallMutArrPtrs_payload"))) uint32_t offset_StgSmallMutArrPtrs_payload() {
return offsetof(StgSmallMutArrPtrs, payload);
}

__attribute__((export_name("offset_StgStack_stack_size"))) uint32_t offset_StgStack_stack_size() { return offsetof(StgStack, stack_size); }

__attribute__((export_name("offset_StgStack_dirty"))) uint32_t offset_StgStack_dirty() { return offsetof(StgStack, dirty); }

__attribute__((export_name("offset_StgStack_sp"))) uint32_t offset_StgStack_sp() { return offsetof(StgStack, sp); }

__attribute__((export_name("offset_StgStack_stack"))) uint32_t offset_StgStack_stack() {
return 4 * ROUNDUP_BYTES_TO_WDS(sizeof(StgStack));
}

__attribute__((export_name("sizeof_StgStopFrame"))) uint32_t sizeof_StgStopFrame() { return sizeof(StgStopFrame); }

__attribute__((export_name("sizeof_StgThunk"))) uint32_t sizeof_StgThunk() { return sizeof(StgThunk); }

__attribute__((export_name("offset_StgThunk_payload"))) uint32_t offset_StgThunk_payload() { return offsetof(StgThunk, payload); }

__attribute__((export_name("offset_StgThunkInfoTable_i"))) uint32_t offset_StgThunkInfoTable_i() { return offsetof(StgThunkInfoTable, i); }

__attribute__((export_name("offset_StgThunkInfoTable_srt"))) uint32_t offset_StgThunkInfoTable_srt() {
return offsetof(StgThunkInfoTable, srt);
}

__attribute__((export_name("sizeof_StgTSO"))) uint32_t sizeof_StgTSO() { return sizeof(StgTSO); }

__attribute__((export_name("offset_StgTSO_stackobj"))) uint32_t offset_StgTSO_stackobj() { return offsetof(StgTSO, stackobj); }

__attribute__((export_name("offset_StgTSO_what_next"))) uint32_t offset_StgTSO_what_next() { return offsetof(StgTSO, what_next); }

__attribute__((export_name("offset_StgTSO_why_blocked"))) uint32_t offset_StgTSO_why_blocked() { return offsetof(StgTSO, why_blocked); }

__attribute__((export_name("offset_StgTSO_flags"))) uint32_t offset_StgTSO_flags() { return offsetof(StgTSO, flags); }

__attribute__((export_name("offset_StgTSO_block_info"))) uint32_t offset_StgTSO_block_info() { return offsetof(StgTSO, block_info); }

__attribute__((export_name("offset_StgTSO_id"))) uint32_t offset_StgTSO_id() { return offsetof(StgTSO, id); }

__attribute__((export_name("offset_StgTSO_saved_errno"))) uint32_t offset_StgTSO_saved_errno() { return offsetof(StgTSO, saved_errno); }

__attribute__((export_name("offset_StgTSO_dirty"))) uint32_t offset_StgTSO_dirty() { return offsetof(StgTSO, dirty); }

__attribute__((export_name("offset_StgTSO_cap"))) uint32_t offset_StgTSO_cap() { return offsetof(StgTSO, cap); }

__attribute__((export_name("offset_StgTSO_blocked_exceptions"))) uint32_t offset_StgTSO_blocked_exceptions() {
return offsetof(StgTSO, blocked_exceptions);
}

__attribute__((export_name("offset_StgTSO_alloc_limit"))) uint32_t offset_StgTSO_alloc_limit() { return offsetof(StgTSO, alloc_limit); }

__attribute__((export_name("offset_StgTSO_tot_stack_size"))) uint32_t offset_StgTSO_tot_stack_size() {
return offsetof(StgTSO, tot_stack_size);
}

__attribute__((export_name("offset_StgUpdateFrame_updatee"))) uint32_t offset_StgUpdateFrame_updatee() {
return offsetof(StgUpdateFrame, updatee);
}

__attribute__((export_name("offset_StgWeak_cfinalizers"))) uint32_t offset_StgWeak_cfinalizers() { return offsetof(StgWeak, cfinalizers); }

__attribute__((export_name("offset_StgWeak_key"))) uint32_t offset_StgWeak_key() { return offsetof(StgWeak, key); }

__attribute__((export_name("offset_StgWeak_value"))) uint32_t offset_StgWeak_value() { return offsetof(StgWeak, value); }

__attribute__((export_name("offset_StgWeak_finalizer"))) uint32_t offset_StgWeak_finalizer() { return offsetof(StgWeak, finalizer); }

__attribute__((export_name("offset_StgWeak_link"))) uint32_t offset_StgWeak_link() { return offsetof(StgWeak, link); }

__attribute__((export_name("next_ThreadRunGHC"))) uint32_t next_ThreadRunGHC() { return ThreadRunGHC; }

__attribute__((export_name("bf_PINNED"))) uint32_t bf_PINNED() { return BF_PINNED; }

__attribute__((export_name("blocked_NotBlocked"))) uint32_t blocked_NotBlocked() { return NotBlocked; }

__attribute__((export_name("blocked_BlockedOnCCall"))) uint32_t blocked_BlockedOnCCall() { return BlockedOnCCall; }

__attribute__((export_name("ret_ThreadBlocked"))) uint32_t ret_ThreadBlocked() { return ThreadBlocked; }

__attribute__((export_name("scheduler_Success"))) uint32_t scheduler_Success() { return Success; }

__attribute__((export_name("sizeof_StgStableName"))) uint32_t sizeof_StgStableName() { return sizeof(StgStableName); }

__attribute__((export_name("offset_StgStableName_header"))) uint32_t offset_StgStableName_header() { return offsetof(StgStableName, header); }

__attribute__((export_name("offset_StgStableName_sn"))) uint32_t offset_StgStableName_sn() { return offsetof(StgStableName, sn); }

__attribute__((export_name("clock_monotonic"))) uint32_t clock_monotonic() { return CLOCK_MONOTONIC; }

__attribute__((export_name("clock_realtime"))) uint32_t clock_realtime() { return CLOCK_REALTIME; }
