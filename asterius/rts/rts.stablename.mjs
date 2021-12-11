import * as rtsConstants from "./rts.constants.mjs";
import { isI32 } from "./rts.typecheck.mjs";

// https://github.com/ghc/ghc/blob/bf73419518ca550e85188616f860961c7e2a336b/includes/rts/StableName.h
// https://github.com/ghc/ghc/blob/43967c0c7d2d0110cfc5f9d64a7dab3a3dda8953/rts/StableName.c
export class StableNameManager {
  constructor(memory, heapalloc, symbol_table) {
    this.memory = memory;
    this.heapalloc = heapalloc;
    this.ptr2stable = new Map();
    this.SymbolTable = symbol_table;
    Object.seal(this);
  }

  makeStableName(ptr) {
    isI32(ptr);
    const oldstable = this.ptr2stable.get(ptr);
    if (oldstable !== undefined) return oldstable;

    const tag = this.ptr2stable.size;

    // https://github.com/ghc/ghc/blob/fe819dd637842fb564524a7cf80612a3673ce14c/includes/rts/storage/Closures.h#L197
    let stableptr = this.heapalloc.allocate(
      0xdeadbeef,
      Math.ceil(rtsConstants.sizeof_StgStableName / 4)
    );
    this.memory.i32Store(stableptr, this.SymbolTable.addressOf("stg_STABLE_NAME_info"));
    this.memory.i32Store(stableptr + rtsConstants.offset_StgStableName_sn, tag);

    this.ptr2stable.set(ptr, stableptr);

    return isI32(stableptr);
  }
}
