import * as rtsConstants from "./rts.constants.mjs";

// https://github.com/ghc/ghc/blob/bf73419518ca550e85188616f860961c7e2a336b/includes/rts/StableName.h
// https://github.com/ghc/ghc/blob/43967c0c7d2d0110cfc5f9d64a7dab3a3dda8953/rts/StableName.c
export class StableNameManager {
  constructor(memory, heapalloc, symbol_table) {
    this.memory = memory;
    this.heapalloc = heapalloc;
    this.ptr2stable = new Map();
    this.stable2ptr = new Map();
    this.SymbolTable = symbol_table;
    Object.freeze(this);
  }

  makeStableName(ptr) {
      const oldstable = this.ptr2stable.get(ptr);
      console.log("makeStableName | ptr: ", ptr, " |this.ptr2tag: ", this.ptr2tag, "oldTag: ", oldstable);
      if (oldstable !== undefined) return oldstable;

      const tag = this.ptr2stable.size;

      // https://github.com/ghc/ghc/blob/fe819dd637842fb564524a7cf80612a3673ce14c/includes/rts/storage/Closures.h#L197
      let stableptr = this.heapalloc.allocatePinned(rtsConstants.sizeof_StgStableName);
      console.log("storing stg_stablename_info: ", this.SymbolTable.stg_STABLE_NAME_info);
      this.memory.i64Store(stableptr, this.SymbolTable.stg_STABLE_NAME_info);
      console.log("storing tag: ", rtsConstants.offset_StgStableName_sn, " ", tag);
      this.memory.i64Store(stableptr + rtsConstants.offset_StgStableName_sn, tag);

      this.ptr2stable.set(ptr, stableptr);
      this.stable2ptr.set(stableptr, ptr);


      return stableptr;
  }

}
