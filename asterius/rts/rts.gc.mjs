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
    Object.freeze(this);
  }

  bdescr(c) {}

  isPinned(c) {}

  allocate(n) {}

  copyClosure(c, size) {}

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
    for (const c in this.pinnedClosures) this.evacuateClosure(c);
    for (const[sp, c] of this.stablePtrManager.spt.entries())
      if (!(sp & 1)) this.stablePtrManager.spt.set(sp, this.evacuateClosure(c));
    this.evacuateClosure(tso);
    this.scavengeWorkList();
    this.closureIndirects.clear();
    this.liveMBlocks.clear();
  }
}
