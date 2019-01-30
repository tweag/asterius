import * as ClosureTypes from "./rts.closuretypes.mjs";
import { Memory } from "./rts.memory.mjs";
import * as settings from "./rts.settings.mjs";

export class GC {
  constructor(memory, mblockalloc, stableptr_manager, info_tables,
              pinned_closures) {
    this.memory = memory;
    this.mblockAlloc = mblockalloc;
    this.stablePtrManager = stableptr_manager;
    this.infoTables = info_tables;
    this.pinnedClosures = pinned_closures;
    this.closureIndirects = new Map();
    this.liveMBlocks = new Set();
    this.workList = [];
    this.currentNursery = undefined;
    Object.seal(this);
  }

  bdescr(c) {
    return (((Number(c) >> Math.log2(settings.mblock_size))
             << Math.log2(settings.mblock_size)) +
            settings.offset_first_bdescr);
  }

  isPinned(c) {
    const bd = this.bdescr(c),
          flags = this.memory.i16Load(bd + settings.offset_bdescr_flags);
    return Boolean(flags & settings.BF_PINNED);
  }

  allocate(bytes) {
    const rounded_bytes = Math.ceil(bytes / 8) << 3;
    while (true) {
      let start = Number(this.memory.i64Load(this.currentNursery +
                                             settings.offset_bdescr_start)),
          free = Number(this.memory.i64Load(this.currentNursery +
                                            settings.offset_bdescr_free)),
          blocks = this.memory.i32Load(this.currentNursery +
                                       settings.offset_bdescr_blocks),
          limit = start + settings.block_size * blocks;
      if (free + rounded_bytes <= limit) {
        this.liveMBlocks.add(this.currentNursery);
        this.memory.i64Store(this.currentNursery + settings.offset_bdescr_free,
                             free + rounded_bytes);
        return free;
      } else {
        this.currentNursery = this.mblockAlloc.allocMegaGroup(1);
        continue;
      }
    }
  }

  copyClosure(c, bytes) {
    const dest_c = this.allocate(bytes);
    this.memory.memcpy(dest_c, c, bytes);
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
            case ClosureTypes.CONSTR:
            case ClosureTypes.CONSTR_1_0:
            case ClosureTypes.CONSTR_0_1:
            case ClosureTypes.CONSTR_2_0:
            case ClosureTypes.CONSTR_1_1:
            case ClosureTypes.CONSTR_0_2:
            case ClosureTypes.CONSTR_NOCAF:
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
            case ClosureTypes.COMPACT_NFDATA:
            case ClosureTypes.THUNK_STATIC:
            case ClosureTypes.THUNK:
            case ClosureTypes.THUNK_1_0:
            case ClosureTypes.THUNK_0_1:
            case ClosureTypes.THUNK_2_0:
            case ClosureTypes.THUNK_1_1:
            case ClosureTypes.THUNK_0_2:
            case ClosureTypes.THUNK_SELECTOR: {
              const ptrs = this.memory.i32Load(
                  info + settings.offset_StgInfoTable_layout),
                    non_ptrs = this.memory.i32Load(
                        info + settings.offset_StgInfoTable_layout + 4);
              dest_c = this.copyClosure(untagged_c, (1 + ptrs + non_ptrs) << 3);
              break;
            }
            case ClosureTypes.AP: {
              const n_args = this.memory.i32Load(untagged_c +
                                                 settings.offset_StgAP_n_args);
              dest_c = this.copyClosure(untagged_c,
                                        settings.sizeof_StgAP + (n_args << 3));
              break;
            }
            case ClosureTypes.PAP: {
              const n_args = this.memory.i32Load(untagged_c +
                                                 settings.offset_StgPAP_n_args);
              dest_c = this.copyClosure(untagged_c,
                                        settings.sizeof_StgPAP + (n_args << 3));
              break;
            }
            case ClosureTypes.AP_STACK: {
              const size = Number(this.memory.i64Load(
                  untagged_c + settings.offset_StgAP_STACK_size));
              dest_c = this.copyClosure(
                  untagged_c, settings.sizeof_StgAP_STACK + (size << 3));
              break;
            }
            case ClosureTypes.IND: {
              return this.evacuateClosure(this.memory.i64Load(
                  untagged_c + settings.offset_StgInd_indirectee));
            }
            case ClosureTypes.IND_STATIC: {
              dest_c =
                  this.copyClosure(untagged_c, settings.sizeof_StgIndStatic);
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
            case ClosureTypes.WEAK: {
              dest_c = this.copyClosure(untagged_c, settings.sizeof_StgWeak);
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

  scavengeWorkList() {
    while (this.workList.length) this.scavengeClosure(this.workList.pop());
  }

  scavengeClosure(c) {}

  gcRootTSO(tso) {
    this.currentNursery = this.mblockAlloc.allocMegaGroup(1);
    for (const c in this.pinnedClosures) this.evacuateClosure(c);
    for (const[sp, c] of this.stablePtrManager.spt.entries())
      if (!(sp & 1)) this.stablePtrManager.spt.set(sp, this.evacuateClosure(c));
    this.evacuateClosure(tso);
    this.scavengeWorkList();
    this.closureIndirects.clear();
    this.liveMBlocks.clear();
    this.currentNursery = undefined;
  }
}
