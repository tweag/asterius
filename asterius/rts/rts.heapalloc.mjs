import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

export class HeapAlloc {
  constructor(memory, mblockalloc) {
    this.memory = memory;
    this.mblockAlloc = mblockalloc;
    this.currentPools = [ undefined, undefined ];
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

  allocMegaGroup(n) {
    const req_blocks = ((rtsConstants.mblock_size * n) - rtsConstants.offset_first_block) / rtsConstants.block_size;
    for (let i = 0; i < this.mblockAlloc.freeList.length; ++i) {
      const bd = this.mblockAlloc.freeList[i],
            blocks = this.memory.i32Load(bd + rtsConstants.offset_bdescr_blocks);
      if (req_blocks < blocks) {
        this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
        const rest_bd = bd + (rtsConstants.mblock_size * n),
              rest_start = rest_bd - rtsConstants.offset_first_bdescr +
                           rtsConstants.offset_first_block;
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_start,
                             rest_start);
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_free, rest_start);
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_link, 0);
        this.memory.i32Store(rest_bd + rtsConstants.offset_bdescr_blocks,
                             blocks - req_blocks -
                                 ((rtsConstants.mblock_size / rtsConstants.block_size) -
                                  rtsConstants.blocks_per_mblock));
        this.mblockAlloc.freeList.splice(i, 1, rest_bd);
        return bd;
      }
      if (req_blocks == blocks) {
        this.mblockAlloc.freeList.splice(i, 1);
        return bd;
      }
    }
    const mblock = this.mblockAlloc.getMBlocks(n),
          bd = mblock + rtsConstants.offset_first_bdescr,
          block_addr = mblock + rtsConstants.offset_first_block;
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
    this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
    return bd;
  }

  preserveMegaGroups(bds) {
    this.mblockAlloc.freeList = [];
    const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
    sorted_bds.push(Memory.tagData(rtsConstants.mblock_size * this.mblockAlloc.capacity) + rtsConstants.offset_first_bdescr);
    this.mblockAlloc.freeSegment(
        Memory.tagData(rtsConstants.mblock_size * this.mblockAlloc.staticMBlocks),
        sorted_bds[0] - rtsConstants.offset_first_bdescr);
    for (let i = 0; i < (sorted_bds.length-1); ++i) {
      const l_start = Number(
          this.memory.i64Load(sorted_bds[i] + rtsConstants.offset_bdescr_start)),
      l_blocks =
          this.memory.i32Load(sorted_bds[i] + rtsConstants.offset_bdescr_blocks),
      l_end = l_start + (rtsConstants.block_size * l_blocks),
      r = sorted_bds[i + 1] - rtsConstants.offset_first_bdescr;
      this.mblockAlloc.freeSegment(l_end, r);
    }
    this.mblockAlloc.freeList.sort(
        (bd0, bd1) => this.memory.i32Load(bd0 + rtsConstants.offset_bdescr_blocks) -
                  this.memory.i32Load(bd1 + rtsConstants.offset_bdescr_blocks));
  }
}
