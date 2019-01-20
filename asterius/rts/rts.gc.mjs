export class GC {
  constructor(memory, mblockalloc, stableptr_manager) {
    this.memory = memory;
    this.mblockAlloc = mblockalloc;
    this.stablePtrManager = stableptr_manager;
    Object.freeze(this);
  }

  gcRootTSO(tso) {}
}
