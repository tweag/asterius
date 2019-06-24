import * as rtsConstants from "./rts.constants.mjs";

// https://github.com/ghc/ghc/blob/bf73419518ca550e85188616f860961c7e2a336b/includes/rts/StableName.h
// https://github.com/ghc/ghc/blob/43967c0c7d2d0110cfc5f9d64a7dab3a3dda8953/rts/StableName.c
export class StableNameManager {
  constructor(memory, heapalloc) {
      this.memory = memory;
    this.heapalloc = heapalloc;
    this.ptr2tag = new Map();
    this.tag2ptr = new Map();
    Object.freeze(this);
  }

  makeStableName(ptr) {
      const oldTag = this.ptr2tag.get(ptr);
      if (oldTag !== undefined) return oldTag;

      const tag = this.ptr2tag.size;
      this.ptr2tag.set(ptr, tag);
      this.tag2ptr.set(tag, ptr);

      // https://github.com/ghc/ghc/blob/fe819dd637842fb564524a7cf80612a3673ce14c/includes/rts/storage/Closures.h#L197
      let stablename = this.heapalloc.allocatePinned(rtsConstants.sizeof_StgStableName);
      this.memory.i64Store(stablename, this.SymbolTable.stg_stablename_info);
      this.memory.i64Store(stableName + rtsConstants.offset_StgStableName_sn, tag);
      return stablename;

  }

}
