import * as rtsConstants from "./rts.constants.mjs";

export class HeapAlloc {
  constructor(memory, mblockalloc) {
    this.memory = memory;
    this.mblockAlloc = mblockalloc;
    this.currentPools = [ undefined, undefined ];
    this.mgroups = new Set();
    Object.freeze(this);
  }
  init() {
    this.currentPools[0] = this.allocMegaGroup(1);
    this.currentPools[1] = this.allocMegaGroup(1);
    this.memory.i16Store(this.currentPools[1] + rtsConstants.offset_bdescr_flags,
                         rtsConstants.BF_PINNED);
  }
  initUnpinned() {
    this.currentPools[0] = this.allocMegaGroup(1);
  }
  hpAlloc(b) {
    const mblocks = b <= rtsConstants.sizeof_first_mblock
                        ? 1
                        : 1 + Math.ceil((b - rtsConstants.sizeof_first_mblock) /
                                        rtsConstants.mblock_size),
          bd = this.allocMegaGroup(mblocks);
    return bd;
  }
  allocate(n, pinned = false) {
    let b = n << 3, pool_i = Number(pinned || b >= rtsConstants.block_size),
        current_start = Number(this.memory.i64Load(
            this.currentPools[pool_i] + rtsConstants.offset_bdescr_start)),
        current_free = Number(this.memory.i64Load(this.currentPools[pool_i] +
                                                  rtsConstants.offset_bdescr_free)),
        current_blocks = this.memory.i32Load(this.currentPools[pool_i] +
                                             rtsConstants.offset_bdescr_blocks),
        current_limit = current_start + rtsConstants.block_size * current_blocks,
        new_free = current_free + b;
    if (new_free <= current_limit) {
      this.memory.i64Store(
          this.currentPools[pool_i] + rtsConstants.offset_bdescr_free, new_free);
      return current_free;
    }
    this.currentPools[pool_i] = this.hpAlloc(b);
    if (pool_i)
      this.memory.i16Store(
          this.currentPools[pool_i] + rtsConstants.offset_bdescr_flags,
          rtsConstants.BF_PINNED);
    current_free = Number(this.memory.i64Load(this.currentPools[pool_i] +
                                              rtsConstants.offset_bdescr_free));
    this.memory.i64Store(
        this.currentPools[pool_i] + rtsConstants.offset_bdescr_free,
        current_free + b);
    return current_free;
  }

  allocatePinned(n) { return this.allocate(n, true); }

  allocMegaGroup(n) {
    const req_blocks =
        (rtsConstants.mblock_size * n - rtsConstants.offset_first_block) /
        rtsConstants.block_size,
      mblock = this.mblockAlloc.getMBlocks(n),
      bd = mblock + rtsConstants.offset_first_bdescr,
      block_addr = mblock + rtsConstants.offset_first_block;
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
    this.memory.i16Store(bd + rtsConstants.offset_bdescr_node, n);
    this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
    this.mgroups.add(bd);
    return bd;
  }

  handleLiveness(live_mblocks, dead_mblocks) {
    for (const bd of live_mblocks) {
      if (!this.mgroups.has(bd)) {
        throw new WebAssembly.RuntimeError(
          `Invalid live mblock 0x${bd.toString(16)}`
        );
      }
    }
    for (const bd of dead_mblocks) {
      if (!this.mgroups.has(bd)) {
        throw new WebAssembly.RuntimeError(
          `Invalid dead mblock 0x${bd.toString(16)}`
        );
      }
      this.mgroups.delete(bd);
      const p = bd - rtsConstants.offset_first_bdescr,
        n = this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
      this.mblockAlloc.free(p, n);
    }
    for (const bd of Array.from(this.mgroups)) {
      if (!live_mblocks.has(bd)) {
        if (
          !(
            this.memory.i16Load(bd + rtsConstants.offset_bdescr_flags) &
            rtsConstants.BF_PINNED
          )
        )
          throw new WebAssembly.RuntimeError(
            `Invalid unpinned mblock 0x${bd.toString(16)}`
          );
        this.mgroups.delete(bd);
        const p = bd - rtsConstants.offset_first_bdescr,
          n = this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
        this.mblockAlloc.free(p, n);
      }
    }
    if (!this.mgroups.has(this.currentPools[0])) {
      throw new WebAssembly.RuntimeError(
        `Invalid unpinned pool 0x${this.currentPools[0].toString(16)}`
      );
    }
    if (!this.mgroups.has(this.currentPools[1])) {
      this.currentPools[1] = this.allocMegaGroup(1);
      this.memory.i16Store(
        this.currentPools[1] + rtsConstants.offset_bdescr_flags,
        rtsConstants.BF_PINNED
      );
    }
  }
}
