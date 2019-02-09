import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as FunTypes from "./rts.funtypes.mjs";
import { Memory } from "./rts.memory.mjs";
import * as settings from "./rts.settings.mjs";
import { stg_arg_bitmaps } from "./rts.autoapply.mjs";

export class GC {
  constructor(memory, mblockalloc, heapalloc, stableptr_manager, info_tables,
              pinned_closures) {
    this.memory = memory;
    this.mblockAlloc = mblockalloc;
    this.heapAlloc = heapalloc;
    this.stablePtrManager = stableptr_manager;
    this.infoTables = info_tables;
    this.pinnedClosures = pinned_closures;
    this.closureIndirects = new Map();
    this.liveMBlocks = new Set();
    this.workList = [];
    Object.freeze(this);
  }

  bdescr(c) {
    return Number(((BigInt(c) >> BigInt(Math.log2(settings.mblock_size)))
                   << BigInt(Math.log2(settings.mblock_size))) |
                  BigInt(settings.offset_first_bdescr));
  }

  isPinned(c) {
    const bd = this.bdescr(c),
          flags = this.memory.i16Load(bd + settings.offset_bdescr_flags);
    return Boolean(flags & settings.BF_PINNED);
  }

  copyClosure(c, bytes) {
    const dest_c = this.heapAlloc.allocate(Math.ceil(bytes / 8));
    this.memory.memcpy(dest_c, c, bytes);
    this.liveMBlocks.add(this.bdescr(dest_c));
    return dest_c;
  }

  evacuateClosure(c) {
    const tag = Memory.getDynTag(c), untagged_c = Memory.unDynTag(c);
    let dest_c = this.closureIndirects.get(untagged_c);
    if (dest_c == undefined) {
      if (this.memory.heapAlloced(untagged_c)) {
        if (this.isPinned(untagged_c)) {
          dest_c = untagged_c;
          this.liveMBlocks.add(this.bdescr(dest_c));
        } else {
          const info = Number(this.memory.i64Load(untagged_c));
          if (!this.infoTables.has(info)) throw new WebAssembly.RuntimeError();
          const type =
              this.memory.i32Load(info + settings.offset_StgInfoTable_type);
          switch (type) {
            case ClosureTypes.CONSTR_0_1:
            case ClosureTypes.FUN_0_1:
            case ClosureTypes.FUN_1_0:
            case ClosureTypes.CONSTR_1_0: {
              dest_c = this.copyClosure(untagged_c, 16);
              break;
            }
            case ClosureTypes.THUNK_1_0:
            case ClosureTypes.THUNK_0_1: {
              dest_c =
                  this.copyClosure(untagged_c, settings.sizeof_StgThunk + 8);
              break;
            }
            case ClosureTypes.THUNK_1_1:
            case ClosureTypes.THUNK_2_0:
            case ClosureTypes.THUNK_0_2: {
              dest_c =
                  this.copyClosure(untagged_c, settings.sizeof_StgThunk + 16);
              break;
            }
            case ClosureTypes.FUN_1_1:
            case ClosureTypes.FUN_2_0:
            case ClosureTypes.FUN_0_2:
            case ClosureTypes.CONSTR_1_1:
            case ClosureTypes.CONSTR_2_0:
            case ClosureTypes.CONSTR_0_2: {
              dest_c = this.copyClosure(untagged_c, 24);
              break;
            }
            case ClosureTypes.THUNK: {
              const ptrs = this.memory.i32Load(
                  info + settings.offset_StgInfoTable_layout),
                    non_ptrs = this.memory.i32Load(
                        info + settings.offset_StgInfoTable_layout + 4);
              dest_c =
                  this.copyClosure(untagged_c, settings.sizeof_StgThunk +
                                                   ((ptrs + non_ptrs) << 3));
              break;
            }
            case ClosureTypes.FUN:
            case ClosureTypes.CONSTR:
            case ClosureTypes.CONSTR_NOCAF:
            case ClosureTypes.MUT_VAR_CLEAN:
            case ClosureTypes.MUT_VAR_DIRTY:
            case ClosureTypes.WEAK:
            case ClosureTypes.PRIM:
            case ClosureTypes.MUT_PRIM:
            case ClosureTypes.BLACKHOLE: {
              const ptrs = this.memory.i32Load(
                  info + settings.offset_StgInfoTable_layout),
                    non_ptrs = this.memory.i32Load(
                        info + settings.offset_StgInfoTable_layout + 4);
              dest_c = this.copyClosure(untagged_c, (1 + ptrs + non_ptrs) << 3);
              break;
            }
            case ClosureTypes.THUNK_SELECTOR: {
              dest_c =
                  this.copyClosure(untagged_c, settings.sizeof_StgSelector);
              break;
            }
            case ClosureTypes.IND: {
              dest_c = this.copyClosure(untagged_c, settings.sizeof_StgInd);
              break;
            }
            case ClosureTypes.PAP: {
              const n_args = this.memory.i32Load(untagged_c +
                                                 settings.offset_StgPAP_n_args);
              dest_c = this.copyClosure(untagged_c,
                                        settings.sizeof_StgPAP + (n_args << 3));
              break;
            }
            case ClosureTypes.AP: {
              const n_args = this.memory.i32Load(untagged_c +
                                                 settings.offset_StgAP_n_args);
              dest_c = this.copyClosure(untagged_c,
                                        settings.sizeof_StgAP + (n_args << 3));
              break;
            }
            case ClosureTypes.AP_STACK: {
              const size = Number(this.memory.i64Load(
                  untagged_c + settings.offset_StgAP_STACK_size));
              dest_c = this.copyClosure(
                  untagged_c, settings.sizeof_StgAP_STACK + (size << 3));
              break;
            }
            case ClosureTypes.ARR_WORDS: {
              dest_c = this.copyClosure(
                  untagged_c,
                  settings.sizeof_StgArrBytes +
                      Number(this.memory.i64Load(
                          untagged_c + settings.offset_StgArrBytes_bytes)));
              break;
            }
            case ClosureTypes.MUT_ARR_PTRS_CLEAN:
            case ClosureTypes.MUT_ARR_PTRS_DIRTY:
            case ClosureTypes.MUT_ARR_PTRS_FROZEN_DIRTY:
            case ClosureTypes.MUT_ARR_PTRS_FROZEN_CLEAN: {
              dest_c = this.copyClosure(
                  untagged_c,
                  settings.sizeof_StgMutArrPtrs +
                      (Number(this.memory.i64Load(
                           untagged_c + settings.offset_StgMutArrPtrs_ptrs))
                       << 3));
              break;
            }
            case ClosureTypes.SMALL_MUT_ARR_PTRS_CLEAN:
            case ClosureTypes.SMALL_MUT_ARR_PTRS_DIRTY:
            case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
            case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: {
              dest_c = this.copyClosure(
                  untagged_c, settings.sizeof_StgSmallMutArrPtrs +
                                  (Number(this.memory.i64Load(
                                       untagged_c +
                                       settings.offset_StgSmallMutArrPtrs_ptrs))
                                   << 3));
              break;
            }
            default:
              throw new WebAssembly.RuntimeError();
          }
        }
      } else {
        dest_c = untagged_c;
      }
      this.closureIndirects.set(untagged_c, dest_c);
      this.workList.push(dest_c);
    }
    return Memory.setDynTag(dest_c, tag);
  }

  scavengeClosureAt(p) {
    this.memory.i64Store(p, this.evacuateClosure(this.memory.i64Load(p)));
  }

  scavengePointersFirst(payload, ptrs) {
    for (let i = 0; i < ptrs; ++i) this.scavengeClosureAt(payload + (i << 3));
  }

  scavengeSmallBitmap(payload, bitmap, size) {
    for (let i = 0; i < size; ++i)
      if (!((bitmap >> BigInt(i)) & BigInt(1)))
        this.scavengeClosureAt(payload + (i << 3));
  }

  scavengeLargeBitmap(payload, large_bitmap, size) {
    for (let j = 0; j < size; j += 64) {
      const bitmap = this.memory.i64Load(
          large_bitmap + settings.offset_StgLargeBitmap_bitmap + (j >> 3));
      for (let i = j; i - j < 64 && i < size; ++i)
        if (!((bitmap >> BigInt(i - j)) & BigInt(1)))
          this.scavengeClosureAt(payload + (i << 3));
    }
  }

  scavengePAP(c, offset_fun, payload, n_args) {
    this.scavengeClosureAt(c + offset_fun);
    const fun = this.memory.i64Load(c + offset_fun),
          fun_info = Number(this.memory.i64Load(fun));
    if (!this.infoTables.has(fun_info)) throw new WebAssembly.RuntimeError();
    switch (this.memory.i32Load(fun_info + settings.offset_StgFunInfoTable_f +
                                settings.offset_StgFunInfoExtraFwd_fun_type)) {
      case FunTypes.ARG_GEN: {
        this.scavengeSmallBitmap(
            payload,
            this.memory.i64Load(fun_info + settings.offset_StgFunInfoTable_f +
                                settings.offset_StgFunInfoExtraFwd_b) >>
                BigInt(6),
            n_args);
        break;
      }
      case FunTypes.ARG_GEN_BIG: {
        this.scavengeLargeBitmap(
            payload,
            Number(this.memory.i64Load(fun_info +
                                       settings.offset_StgFunInfoTable_f +
                                       settings.offset_StgFunInfoExtraFwd_b)),
            n_args);
        break;
      }
      case FunTypes.ARG_BCO: {
        throw new WebAssembly.RuntimeError();
      }
      default: {
        this.scavengeSmallBitmap(
            payload,
            BigInt(stg_arg_bitmaps[this.memory.i32Load(
                fun_info + settings.offset_StgFunInfoTable_f +
                settings.offset_StgFunInfoExtraFwd_fun_type)]) >>
                BigInt(6),
            n_args);
        break;
      }
    }
  }

  scavengeStackChunk(sp, sp_lim) {
    let c = sp;
    while (true) {
      if (c > sp_lim) throw new WebAssembly.RuntimeError();
      if (c == sp_lim) break;
      const info = Number(this.memory.i64Load(c)),
            type =
                this.memory.i32Load(info + settings.offset_StgInfoTable_type),
            raw_layout =
                this.memory.i64Load(info + settings.offset_StgInfoTable_layout);
      if (!this.infoTables.has(info)) throw new WebAssembly.RuntimeError();
      if (this.memory.i32Load(info + settings.offset_StgInfoTable_srt))
        this.evacuateClosure(
            this.memory.i64Load(info + settings.offset_StgRetInfoTable_srt));
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
          this.scavengeSmallBitmap(c + 8, bitmap, size);
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_BIG: {
          const size = Number(this.memory.i64Load(
              Number(raw_layout) + settings.offset_StgLargeBitmap_size));
          this.scavengeLargeBitmap(c + 8, Number(raw_layout), size);
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_FUN: {
          const size =
              Number(this.memory.i64Load(c + settings.offset_StgRetFun_size)),
                fun_info = Number(this.memory.i64Load(
                    this.memory.i64Load(c + settings.offset_StgRetFun_fun)));
          switch (this.memory.i32Load(
              fun_info + settings.offset_StgFunInfoTable_f +
              settings.offset_StgFunInfoExtraFwd_fun_type)) {
            case FunTypes.ARG_GEN: {
              this.scavengeSmallBitmap(
                  c + settings.offset_StgRetFun_payload,
                  this.memory.i64Load(fun_info +
                                      settings.offset_StgFunInfoTable_f +
                                      settings.offset_StgFunInfoExtraFwd_b) >>
                      BigInt(6),
                  size);
              break;
            }
            case FunTypes.ARG_GEN_BIG: {
              this.scavengeLargeBitmap(
                  c + settings.offset_StgRetFun_payload,
                  Number(this.memory.i64Load(
                      fun_info + settings.offset_StgFunInfoTable_f +
                      settings.offset_StgFunInfoExtraFwd_b)),
                  size);
              break;
            }
            case FunTypes.ARG_BCO: {
              throw new WebAssembly.RuntimeError();
            }
            default: {
              this.scavengeSmallBitmap(
                  c + settings.offset_StgRetFun_payload,
                  BigInt(stg_arg_bitmaps[this.memory.i32Load(
                      fun_info + settings.offset_StgFunInfoTable_f +
                      settings.offset_StgFunInfoExtraFwd_fun_type)]) >>
                      BigInt(6),
                  size);
              break;
            }
          }
          c += settings.sizeof_StgRetFun + (size << 3);
          break;
        }
        default:
          throw new WebAssembly.RuntimeError();
      }
    }
  }

  scavengeWorkList() {
    while (this.workList.length) this.scavengeClosure(this.workList.pop());
  }

  scavengeClosure(c) {
    const info = Number(this.memory.i64Load(c)),
          type = this.memory.i32Load(info + settings.offset_StgInfoTable_type);
    if (!this.infoTables.has(info)) throw new WebAssembly.RuntimeError();
    switch (type) {
      case ClosureTypes.CONSTR:
      case ClosureTypes.CONSTR_1_0:
      case ClosureTypes.CONSTR_0_1:
      case ClosureTypes.CONSTR_2_0:
      case ClosureTypes.CONSTR_1_1:
      case ClosureTypes.CONSTR_0_2:
      case ClosureTypes.CONSTR_NOCAF: {
        const ptrs =
            this.memory.i32Load(info + settings.offset_StgInfoTable_layout);
        this.scavengePointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.FUN:
      case ClosureTypes.FUN_1_0:
      case ClosureTypes.FUN_0_1:
      case ClosureTypes.FUN_2_0:
      case ClosureTypes.FUN_1_1:
      case ClosureTypes.FUN_0_2:
      case ClosureTypes.FUN_STATIC: {
        if (this.memory.i32Load(info + settings.offset_StgInfoTable_srt))
          this.evacuateClosure(
              this.memory.i64Load(info + settings.offset_StgFunInfoTable_f +
                                  settings.offset_StgFunInfoExtraFwd_srt));
        const ptrs =
            this.memory.i32Load(info + settings.offset_StgInfoTable_layout);
        this.scavengePointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.BLACKHOLE:
      case ClosureTypes.MUT_VAR_CLEAN:
      case ClosureTypes.MUT_VAR_DIRTY:
      case ClosureTypes.PRIM:
      case ClosureTypes.MUT_PRIM:
      case ClosureTypes.COMPACT_NFDATA: {
        const ptrs =
            this.memory.i32Load(info + settings.offset_StgInfoTable_layout);
        this.scavengePointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.THUNK_STATIC:
      case ClosureTypes.THUNK:
      case ClosureTypes.THUNK_1_0:
      case ClosureTypes.THUNK_0_1:
      case ClosureTypes.THUNK_2_0:
      case ClosureTypes.THUNK_1_1:
      case ClosureTypes.THUNK_0_2: {
        if (this.memory.i32Load(info + settings.offset_StgInfoTable_srt))
          this.evacuateClosure(this.memory.i64Load(
              info + settings.offset_StgThunkInfoTable_srt));
        const ptrs =
            this.memory.i32Load(info + settings.offset_StgInfoTable_layout);
        this.scavengePointersFirst(c + settings.offset_StgThunk_payload, ptrs);
        break;
      }
      case ClosureTypes.THUNK_SELECTOR: {
        if (this.memory.i32Load(info + settings.offset_StgInfoTable_srt))
          this.evacuateClosure(this.memory.i64Load(
              info + settings.offset_StgThunkInfoTable_srt));
        this.scavengeClosureAt(c + settings.offset_StgSelector_selectee);
        break;
      }
      case ClosureTypes.AP: {
        this.scavengePAP(c, settings.offset_StgAP_fun,
                         c + settings.offset_StgAP_payload,
                         this.memory.i32Load(c + settings.offset_StgAP_n_args));
        break;
      }
      case ClosureTypes.PAP: {
        this.scavengePAP(
            c, settings.offset_StgPAP_fun, c + settings.offset_StgPAP_payload,
            this.memory.i32Load(c + settings.offset_StgPAP_n_args));
        break;
      }
      case ClosureTypes.AP_STACK: {
        this.scavengeClosureAt(c + settings.offset_StgAP_STACK_fun);
        this.scavengeStackChunk(c + settings.offset_StgAP_STACK_payload,
                                c + settings.offset_StgAP_STACK_payload +
                                    Number(this.memory.i64Load(
                                        c + settings.offset_StgAP_STACK_size)));
        break;
      }
      case ClosureTypes.IND: {
        this.scavengeClosure(c + settings.offset_StgInd_indirectee);
        break;
      }
      case ClosureTypes.IND_STATIC: {
        this.scavengeClosureAt(c + settings.offset_StgIndStatic_indirectee);
        break;
      }
      case ClosureTypes.ARR_WORDS: {
        break;
      }
      case ClosureTypes.MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_CLEAN: {
        const ptrs =
            Number(this.memory.i64Load(c + settings.offset_StgMutArrPtrs_ptrs));
        this.scavengePointersFirst(c + settings.offset_StgMutArrPtrs_payload,
                                   ptrs);
        break;
      }
      case ClosureTypes.WEAK: {
        this.scavengeClosureAt(c + settings.offset_StgWeak_cfinalizers);
        this.scavengeClosureAt(c + settings.offset_StgWeak_key);
        this.scavengeClosureAt(c + settings.offset_StgWeak_value);
        this.scavengeClosureAt(c + settings.offset_StgWeak_finalizer);
        break;
      }
      case ClosureTypes.TSO: {
        this.scavengeClosureAt(c + settings.offset_StgTSO_stackobj);
        break;
      }
      case ClosureTypes.STACK: {
        const stack_size =
            this.memory.i32Load(c + settings.offset_StgStack_stack_size),
              sp = Number(this.memory.i64Load(c + settings.offset_StgStack_sp)),
              sp_lim = c + settings.offset_StgStack_stack + (stack_size << 3);
        this.scavengeStackChunk(sp, sp_lim);
        break;
      }
      case ClosureTypes.SMALL_MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: {
        this.scavengePointersFirst(
            c + settings.offset_StgSmallMutArrPtrs_payload,
            Number(this.memory.i64Load(
                c + settings.offset_StgSmallMutArrPtrs_ptrs)));
        break;
      }
      default:
        throw new WebAssembly.RuntimeError();
    }
  }

  gcRootTSO(tso) {
    console.log(`[EVENT] gcRootTSO 0x${tso.toString(16)}`);
    this.heapAlloc.init();
    for (const c of this.pinnedClosures) this.evacuateClosure(c);
    for (const[sp, c] of this.stablePtrManager.spt.entries())
      if (!(sp & 1)) this.stablePtrManager.spt.set(sp, this.evacuateClosure(c));
    this.evacuateClosure(tso);
    this.scavengeWorkList();
    this.mblockAlloc.preserveMegaGroups(this.liveMBlocks);
    this.closureIndirects.clear();
    this.liveMBlocks.clear();
  }
}
