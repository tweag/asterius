import * as settings from "./rts.settings.mjs";

export class HeapAlloc {
  constructor(memory, mblockalloc) {
    this.memory = memory;
    this.mblockAlloc = mblockalloc;
    this.currentObjectPool = undefined;
    this.currentPinnedObjectPool = undefined;
    Object.seal(this);
  }
  init() {
    this.currentObjectPool = this.mblockAlloc.allocMegaGroup(1);
    this.currentPinnedObjectPool = this.mblockAlloc.allocMegaGroup(1);
    this.memory.i16Store(
        this.currentPinnedObjectPool + settings.offset_bdescr_flags,
        settings.BF_PINNED);
  }
  hpAlloc(b) {
    return this.mblockAlloc.allocMegaGroup(
        b <= settings.sizeof_first_mblock
            ? 1
            : 1 + Math.ceil((b - settings.sizeof_first_mblock) /
                            settings.mblock_size));
  }
  allocate(n) {
    let b = n << 3,
        current_start = Number(this.memory.i64Load(
            this.currentObjectPool + settings.offset_bdescr_start)),
        current_free = Number(this.memory.i64Load(this.currentObjectPool +
                                                  settings.offset_bdescr_free)),
        current_blocks = this.memory.i32Load(this.currentObjectPool +
                                             settings.offset_bdescr_blocks),
        current_limit = current_start + settings.block_size * current_blocks,
        new_free = current_free + b;
    if (new_free <= current_limit) {
      this.memory.i64Store(this.currentObjectPool + settings.offset_bdescr_free,
                           new_free);
      return current_free;
    }
    this.currentObjectPool = this.mblockAlloc.allocMegaGroup(
        b <= settings.sizeof_first_mblock
            ? 1
            : 1 + Math.ceil((b - settings.sizeof_first_mblock) /
                            settings.mblock_size));
    current_free = Number(this.memory.i64Load(this.currentObjectPool +
                                              settings.offset_bdescr_free));
    this.memory.i64Store(this.currentObjectPool + settings.offset_bdescr_free,
                         current_free + b);
    return current_free;
  }
  allocatePinned(n) {
    let b = n << 3,
        current_start = Number(this.memory.i64Load(
            this.currentPinnedObjectPool + settings.offset_bdescr_start)),
        current_free = Number(this.memory.i64Load(this.currentPinnedObjectPool +
                                                  settings.offset_bdescr_free)),
        current_blocks = this.memory.i32Load(this.currentPinnedObjectPool +
                                             settings.offset_bdescr_blocks),
        current_limit = current_start + settings.block_size * current_blocks,
        new_free = current_free + b;
    if (new_free <= current_limit) {
      this.memory.i64Store(
          this.currentPinnedObjectPool + settings.offset_bdescr_free, new_free);
      return current_free;
    }
    this.currentPinnedObjectPool = this.mblockAlloc.allocMegaGroup(
        b <= settings.sizeof_first_mblock
            ? 1
            : 1 + Math.ceil((b - settings.sizeof_first_mblock) /
                            settings.mblock_size));
    this.memory.i16Store(
        this.currentPinnedObjectPool + settings.offset_bdescr_flags,
        settings.BF_PINNED);
    current_free = Number(this.memory.i64Load(this.currentPinnedObjectPool +
                                              settings.offset_bdescr_free));
    this.memory.i64Store(
        this.currentPinnedObjectPool + settings.offset_bdescr_free,
        current_free + b);
    return current_free;
  }
}
