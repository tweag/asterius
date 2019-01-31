import * as settings from "./rts.settings.mjs";

export class HeapAlloc {
  constructor(memory, mblockalloc) {
    this.memory = memory;
    this.mblockAlloc = mblockalloc;
    this.currentPools = [ undefined, undefined ];
    Object.freeze(this);
  }
  init() {
    this.currentPools[0] = this.mblockAlloc.allocMegaGroup(1);
    this.currentPools[1] = this.mblockAlloc.allocMegaGroup(1);
    this.memory.i16Store(this.currentPools[1] + settings.offset_bdescr_flags,
                         settings.BF_PINNED);
  }
  hpAlloc(b) {
    const mblocks = b <= settings.sizeof_first_mblock
                        ? 1
                        : 1 + Math.ceil((b - settings.sizeof_first_mblock) /
                                        settings.mblock_size),
          bd = this.mblockAlloc.allocMegaGroup(mblocks);
    if (mblocks > 1)
      this.memory.i16Store(bd + settings.offset_bdescr_flags,
                           settings.BF_PINNED);
    return bd;
  }
  allocate(n, pinned = false) {
    let pool_i = Number(pinned), b = n << 3,
        current_start = Number(this.memory.i64Load(
            this.currentPools[pool_i] + settings.offset_bdescr_start)),
        current_free = Number(this.memory.i64Load(this.currentPools[pool_i] +
                                                  settings.offset_bdescr_free)),
        current_blocks = this.memory.i32Load(this.currentPools[pool_i] +
                                             settings.offset_bdescr_blocks),
        current_limit = current_start + settings.block_size * current_blocks,
        new_free = current_free + b;
    if (new_free <= current_limit) {
      this.memory.i64Store(
          this.currentPools[pool_i] + settings.offset_bdescr_free, new_free);
      return current_free;
    }
    this.currentPools[pool_i] = this.hpAlloc(b);
    if (pinned)
      this.memory.i16Store(
          this.currentPinnedObjectPool + settings.offset_bdescr_flags,
          settings.BF_PINNED);
    current_free = Number(this.memory.i64Load(this.currentPools[pool_i] +
                                              settings.offset_bdescr_free));
    this.memory.i64Store(
        this.currentPools[pool_i] + settings.offset_bdescr_free,
        current_free + b);
    return current_free;
  }
  allocatePinned(n) { return this.allocate(n, true); }
}
