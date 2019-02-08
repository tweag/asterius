import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as FunTypes from "./rts.funtypes.mjs";
import { Memory } from "./rts.memory.mjs";
import * as settings from "./rts.settings.mjs";
import { stg_arg_bitmaps } from "./rts.autoapply.mjs";

export class SanCheck {
  constructor(memory, stableptr_mgr, info_tables, pinned_closures) {
    this.memory = memory;
    this.stablePtrManager = stableptr_mgr;
    this.infoTables = info_tables;
    this.pinnedClosures = pinned_closures;
    this.visitedClosures = new Set();
    this.visitedBdescrs = new Set();
    this.workList = [];
    Object.freeze(this);
  }

  checkInfoTable(p) {
    if (!this.infoTables.has(Number(p)))
      throw new WebAssembly.RuntimeError(`Invalid info table: 0x${ p.toString(16) }`);
  }

  checkPointersFirst(payload, ptrs) {
    for (let i = 0; i < Number(ptrs); ++i)
      this.enqueueClosure(this.memory.i64Load(payload + (i << 3)));
  }

  checkSmallBitmap(payload, bitmap, size) {
    for (let i = 0; i < Number(size); ++i)
      if (!((bitmap >> BigInt(i)) & BigInt(1)))
        this.enqueueClosure(this.memory.i64Load(Number(payload) + (i << 3)));
  }

  checkLargeBitmap(payload, large_bitmap, size) {
    for (let j = 0; j < Number(size); j += 64) {
      const bitmap = this.memory.i64Load(
          large_bitmap + settings.offset_StgLargeBitmap_bitmap + (j >> 3));
      for (let i = j; i - j < 64 && i < Number(size); ++i)
        if (!((bitmap >> BigInt(i - j)) & BigInt(1)))
          this.enqueueClosure(this.memory.i64Load(Number(payload) + (i << 3)));
    }
  }

  checkPAP(fun, payload, n_args) {
    this.enqueueClosure(fun);
    const fun_info = Number(this.memory.i64Load(fun));
    this.checkInfoTable(fun_info);
    switch(this.memory.i32Load(fun_info + settings.offset_StgFunInfoTable_f +
                                  settings.offset_StgFunInfoExtraFwd_fun_type)) {
      case FunTypes.ARG_GEN:
        this.checkSmallBitmap(payload, this.memory.i64Load(fun_info +
                                      settings.offset_StgFunInfoTable_f +
                                      settings.offset_StgFunInfoExtraFwd_b) >>
                      BigInt(6), n_args);
        break;
      case FunTypes.ARG_GEN_BIG:
        this.checkLargeBitmap(payload, this.memory.i64Load(fun_info +
                                      settings.offset_StgFunInfoTable_f +
                                      settings.offset_StgFunInfoExtraFwd_b), n_args);
        break;
      case FunTypes.ARG_BCO:
        throw new WebAssembly.RuntimeError(`Invalid ARG_BCO in closure 0x${ fun.toString(16) }`);
      default:
        this.checkSmallBitmap(
            payload,
            BigInt(stg_arg_bitmaps
                       [this.memory.i32Load(
                            fun_info + settings.offset_StgFunInfoTable_f +
                            settings.offset_StgFunInfoExtraFwd_fun_type)]) >>
                BigInt(6),
            n_args);
        break;
    }
  }

  enqueueClosure(_c) {
    const c = Number(_c), untagged_c = Memory.unDynTag(c);
    if (this.visitedClosures.has(untagged_c)) return;
    this.visitedClosures.add(untagged_c);
    this.workList.push(c);
  }

  checkWorkList() {
    while(this.workList.length)
      this.checkClosure(this.workList.pop());
  }

  checkClosure(c) {
    if (this.memory.heapAlloced(c))
      this.visitClosureBdescr(c);
    const p = Number(this.memory.i64Load(c)),
          type = this.memory.i32Load(p + settings.offset_StgInfoTable_type);
    this.checkInfoTable(p);
    switch (type) {
      case ClosureTypes.CONSTR:
      case ClosureTypes.CONSTR_1_0:
      case ClosureTypes.CONSTR_0_1:
      case ClosureTypes.CONSTR_2_0:
      case ClosureTypes.CONSTR_1_1:
      case ClosureTypes.CONSTR_0_2:
      case ClosureTypes.CONSTR_NOCAF: {
        const ptrs = this.memory.i32Load(p + settings.offset_StgInfoTable_layout);
        this.checkPointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.FUN:
      case ClosureTypes.FUN_1_0:
      case ClosureTypes.FUN_0_1:
      case ClosureTypes.FUN_2_0:
      case ClosureTypes.FUN_1_1:
      case ClosureTypes.FUN_0_2:
      case ClosureTypes.FUN_STATIC:
      case ClosureTypes.BLACKHOLE:
      case ClosureTypes.MUT_VAR_CLEAN:
      case ClosureTypes.MUT_VAR_DIRTY:
      case ClosureTypes.PRIM:
      case ClosureTypes.MUT_PRIM:
      case ClosureTypes.COMPACT_NFDATA: {
        if (this.memory.i32Load(p + settings.offset_StgInfoTable_srt))
          this.enqueueClosure(this.memory.i64Load(p + settings.offset_StgFunInfoTable_f + settings.offset_StgFunInfoExtraFwd_srt));
        const ptrs = this.memory.i32Load(p + settings.offset_StgInfoTable_layout);
        this.checkPointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.THUNK_STATIC: {
        if (this.memory.i32Load(p + settings.offset_StgInfoTable_srt))
          this.enqueueClosure(this.memory.i64Load(p + settings.offset_StgThunkInfoTable_srt));
        const ptrs = this.memory.i32Load(p + settings.offset_StgInfoTable_layout);
        this.checkPointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.THUNK:
      case ClosureTypes.THUNK_1_0:
      case ClosureTypes.THUNK_0_1:
      case ClosureTypes.THUNK_2_0:
      case ClosureTypes.THUNK_1_1:
      case ClosureTypes.THUNK_0_2: {
        if (this.memory.i32Load(p + settings.offset_StgInfoTable_srt))
          this.enqueueClosure(this.memory.i64Load(p + settings.offset_StgThunkInfoTable_srt));
        const ptrs = this.memory.i32Load(p + settings.offset_StgInfoTable_layout);
        this.checkPointersFirst(c + settings.offset_StgThunk_payload, ptrs);
        break;
      }
      case ClosureTypes.THUNK_SELECTOR: {
        if (this.memory.i32Load(p + settings.offset_StgInfoTable_srt))
          this.enqueueClosure(this.memory.i64Load(p + settings.offset_StgThunkInfoTable_srt));
        this.enqueueClosure(
            this.memory.i64Load(c + settings.offset_StgSelector_selectee));
        break;
      }
      case ClosureTypes.BCO:
        throw new WebAssembly.RuntimeError(`Invalid BCO object at 0x${c.toString(16)}`);
      case ClosureTypes.AP: {
        this.checkPAP(
            this.memory.i64Load(c + settings.offset_StgAP_fun),
            c + settings.offset_StgAP_payload,
            this.memory.i32Load(c + settings.offset_StgAP_n_args));
        break;
      }
      case ClosureTypes.PAP: {
        this.checkPAP(
            this.memory.i64Load(c + settings.offset_StgPAP_fun),
            c + settings.offset_StgPAP_payload,
            this.memory.i32Load(c + settings.offset_StgPAP_n_args));
        break;
      }
      case ClosureTypes.AP_STACK: {
        this.enqueueClosure(
            this.memory.i64Load(c + settings.offset_StgAP_STACK_fun));
        this.enqueueStackChunk(c + settings.offset_StgAP_STACK_payload,
                             c + settings.offset_StgAP_STACK_payload +
                                 Number(this.memory.i64Load(
                                     c + settings.offset_StgAP_STACK_size)));
        break;
      }
      case ClosureTypes.IND: {
        this.enqueueClosure(
            this.memory.i64Load(c + settings.offset_StgInd_indirectee));
        break;
      }
      case ClosureTypes.IND_STATIC: {
        this.enqueueClosure(
            this.memory.i64Load(c + settings.offset_StgIndStatic_indirectee));
        break;
      }
      case ClosureTypes.RET_BCO:
      case ClosureTypes.RET_SMALL:
      case ClosureTypes.RET_BIG:
      case ClosureTypes.RET_FUN:
      case ClosureTypes.UPDATE_FRAME:
      case ClosureTypes.CATCH_FRAME:
      case ClosureTypes.UNDERFLOW_FRAME:
      case ClosureTypes.STOP_FRAME:
      case ClosureTypes.ATOMICALLY_FRAME:
      case ClosureTypes.CATCH_RETRY_FRAME:
      case ClosureTypes.CATCH_STM_FRAME:
        throw new WebAssembly.RuntimeError(`Invalid stack frame on the heap at 0x${ c.toString(16) }`);
      case ClosureTypes.BLOCKING_QUEUE:
        throw new WebAssembly.RuntimeError(`Invalid blocking queue at 0x${ c.toString(16) }`);
      case ClosureTypes.MVAR_CLEAN:
      case ClosureTypes.MVAR_DIRTY:
        throw new WebAssembly.RuntimeError(`Unsupported MVar at 0x${ c.toString(16) }`);
      case ClosureTypes.TVAR:
        throw new WebAssembly.RuntimeError(`Unsupported TVar at 0x${ c.toString(16) }`);
      case ClosureTypes.ARR_WORDS:
        break;
      case ClosureTypes.MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_CLEAN: {
        const ptrs =
            Number(this.memory.i64Load(c + settings.offset_StgMutArrPtrs_ptrs));
        this.checkPointersFirst(c + settings.offset_StgMutArrPtrs_payload,
                                ptrs);
        break;
      }
      case ClosureTypes.WEAK: {
        this.enqueueClosure(
            this.memory.i64Load(c + settings.offset_StgWeak_cfinalizers));
        this.enqueueClosure(this.memory.i64Load(c + settings.offset_StgWeak_key));
        this.enqueueClosure(
            this.memory.i64Load(c + settings.offset_StgWeak_value));
        this.enqueueClosure(
            this.memory.i64Load(c + settings.offset_StgWeak_finalizer));
        const link = this.memory.i64Load(c + settings.offset_StgWeak_link);
        if (link) this.enqueueClosure(link);
        break;
      }
      case ClosureTypes.TSO: {
        const stackobj =
              Number(this.memory.i64Load(c + settings.offset_StgTSO_stackobj));
        this.enqueueClosure(stackobj);
        break;
      }
      case ClosureTypes.STACK: {
        const stack_size = this.memory.i32Load(c + settings.offset_StgStack_stack_size),
          sp = Number(this.memory.i64Load(c + settings.offset_StgStack_sp)),
          sp_lim = c + settings.offset_StgStack_stack + (stack_size << 3);
        this.enqueueStackChunk(sp, sp_lim);
        break;
      }
      case ClosureTypes.TREC_CHUNK:
        throw new WebAssembly.RuntimeError(`Invalid TREC_CHUNK at 0x${ c.toString(16) }`);
      case ClosureTypes.SMALL_MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: {
        this.checkPointersFirst(
            c + settings.offset_StgSmallMutArrPtrs_payload,
            this.memory.i64Load(c + settings.offset_StgSmallMutArrPtrs_ptrs));
        break;
      }
      default:
        throw new WebAssembly.RuntimeError(`Invalid closure type ${ type } for closure 0x${ c.toString(16) }`);
    }
  }

  enqueueStackChunk(sp, sp_lim) {
    let c = sp;
    while (true) {
      if (c > sp_lim)
        throw new WebAssembly.RuntimeError(`Slipped through SpLim,
                                           SpLim: 0x${ sp_lim.toString(16) },
                                           Sp: 0x${ c.toString(16) }`);
      if (c === sp_lim) break;
      const p = Number(this.memory.i64Load(c)),
            type = this.memory.i32Load(p + settings.offset_StgInfoTable_type),
            raw_layout =
                this.memory.i64Load(p + settings.offset_StgInfoTable_layout);
      this.checkInfoTable(p);
      if (this.memory.i32Load(p + settings.offset_StgInfoTable_srt))
          this.enqueueClosure(this.memory.i64Load(p + settings.offset_StgRetInfoTable_srt));
      switch (type) {
        case ClosureTypes.RET_SMALL:
        case ClosureTypes.UPDATE_FRAME:
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.UNDERFLOW_FRAME:
        case ClosureTypes.STOP_FRAME:
        case ClosureTypes.ATOMICALLY_FRAME:
        case ClosureTypes.CATCH_RETRY_FRAME:
        case ClosureTypes.CATCH_STM_FRAME: {
          const size = Number(raw_layout & BigInt(0x3f)),
                bitmap = raw_layout >> BigInt(6);
          this.checkSmallBitmap(c + 8, bitmap, size);
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_BIG: {
          const size = Number(this.memory.i64Load(
              Number(raw_layout) + settings.offset_StgLargeBitmap_size));
          this.checkLargeBitmap(c + 8, Number(raw_layout), size);
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_FUN: {
          const size =
              Number(this.memory.i64Load(c + settings.offset_StgRetFun_size)),
                fun_info = Number(this.memory.i64Load(
                    this.memory.i64Load(c + settings.offset_StgRetFun_fun)));
          switch (this.memory.i32Load(fun_info + settings.offset_StgFunInfoTable_f +
                                  settings.offset_StgFunInfoExtraFwd_fun_type)) {
            case FunTypes.ARG_GEN:
              this.checkSmallBitmap(
                  c + settings.offset_StgRetFun_payload,
                  this.memory.i64Load(fun_info +
                                      settings.offset_StgFunInfoTable_f +
                                      settings.offset_StgFunInfoExtraFwd_b) >>
                      BigInt(6),
                  size);
              break;
            case FunTypes.ARG_GEN_BIG:
              this.checkLargeBitmap(
                  c + settings.offset_StgRetFun_payload,
                  this.memory.i64Load(fun_info +
                                      settings.offset_StgFunInfoTable_f +
                                      settings.offset_StgFunInfoExtraFwd_b),
                  size);
              break;
            case FunTypes.ARG_BCO:
              throw new WebAssembly.RuntimeError(`Invalid ARG_BCO in RET_FUN frame 0x${ c.toString(16) }`);
            default:
              this.checkSmallBitmap(
                  c + settings.offset_StgRetFun_payload,
                  BigInt(stg_arg_bitmaps
                             [this.memory.i32Load(
                                  fun_info + settings.offset_StgFunInfoTable_f +
                                  settings.offset_StgFunInfoExtraFwd_fun_type)]) >>
                      BigInt(6),
                  size);
              break;
          }
          c += settings.sizeof_StgRetFun + (size << 3);
          break;
        }
        default:
          throw new WebAssembly.RuntimeError(`Invalid frame type ${ tbl.type }`);
      }
    }
  }

  enqueueStablePtrs() {
    for (const [sp, addr] of this.stablePtrManager.spt.entries())
      if (!(sp & 1)) this.enqueueClosure(addr);
  }

  visitClosureBdescr(c) {
    const bd = Number(((BigInt(c) >> BigInt(Math.log2(settings.mblock_size)) )
             << BigInt(Math.log2(settings.mblock_size)) ) |
            BigInt(settings.offset_first_bdescr));
    this.visitedBdescrs.add(bd);
  }

  checkRootTSO(i, tso) {
    console.log(`[EVENT] checkRootTSO ${i}`);
    try {
      for (const c of this.pinnedClosures)
        this.enqueueClosure(c);
      this.enqueueStablePtrs();
      this.enqueueClosure(tso);
      this.checkWorkList();
    } catch (err) {
      throw new WebAssembly.RuntimeError(`Captured error: ${err.stack}\n`);
    }
    console.log(`[EVENT] Live object count: ${ this.visitedClosures.size }, live mgroup count: ${ this.visitedBdescrs.size }`);
    this.visitedClosures.clear();
    this.visitedBdescrs.clear();
  }
}
