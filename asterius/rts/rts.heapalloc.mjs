import * as rtsConstants from "./rts.constants.mjs";

export class HeapAlloc {
  constructor(memory, blockalloc, mblockalloc) {
    this.memory = memory;
    // this.mblockAlloc = mblockalloc;
    this.blockAlloc = blockalloc;
    this.currentPools = [ undefined, undefined ];
    Object.freeze(this);
  }
  init() {
    this.currentPools[0] = this.blockAlloc.allocBlocks(1 * rtsConstants.blocks_per_mblock);
    this.currentPools[1] = this.blockAlloc.allocBlocks(1 * rtsConstants.blocks_per_mblock);
    this.memory.i16Store(this.currentPools[1] + rtsConstants.offset_bdescr_flags,
                         rtsConstants.BF_PINNED);
  }
  initUnpinned() {
    this.currentPools[0] = this.blockAlloc.allocBlocks(1 * rtsConstants.blocks_per_mblock);
  }
  hpAlloc(b) {
    // const mblocks = b <= rtsConstants.sizeof_first_mblock
    //                     ? 1
    //                     : 1 + Math.ceil((b - rtsConstants.sizeof_first_mblock) /
    //                                     rtsConstants.mblock_size),
    //       bd = this.blockAlloc.allocBlocks(mblocks * rtsConstants.blocks_per_mblock);
    const blocks = Math.min(1, Math.ceil(b / rtsConstants.block_size));
    const mblocks = blocks / rtsConstants.blocks_per_mblock;
    const bd = this.blockAlloc.allocBlocks(blocks);

    if (mblocks > 1)
      this.memory.i16Store(bd + rtsConstants.offset_bdescr_flags,
                           rtsConstants.BF_PINNED);
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
}
