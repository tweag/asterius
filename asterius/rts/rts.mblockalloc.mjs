import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

export class MBlockAlloc {
  constructor() {
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.capacity = undefined;
    this.size = undefined;
    this.freeList = [];
    Object.seal(this);
  }

  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.capacity = this.memory.buffer.byteLength / rtsConstants.mblock_size;
    this.size = this.capacity;
  }

  getMBlocks(n) {
    if (this.size + n > this.capacity) {
      const d = Math.max(n, this.capacity);
      this.memory.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    return Memory.tagData(prev_size * rtsConstants.mblock_size);
  }

  allocMegaGroup(n) {
    const req_blocks = ((rtsConstants.mblock_size * n) - rtsConstants.offset_first_block) / rtsConstants.block_size;
    /*
    for (let i = 0; i < this.freeList.length; ++i) {
      const bd = this.freeList[i],
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
        this.freeList.splice(i, 1, rest_bd);
        return bd;
      }
      if (req_blocks == blocks) {
        this.freeList.splice(i, 1);
        return bd;
      }
    }
    */
    const mblock = this.getMBlocks(n),
          bd = mblock + rtsConstants.offset_first_bdescr,
          block_addr = mblock + rtsConstants.offset_first_block;
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
    this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
    return bd;
  }

  freeSegment(l_end, r) {
    if (l_end < r) {
      this.memory.memset(l_end, 0x42, r - l_end);
      const bd = l_end + rtsConstants.offset_first_bdescr,
            start = l_end + rtsConstants.offset_first_block,
            blocks = (r - start) / rtsConstants.block_size;
      this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, start);
      this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, start);
      this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
      this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, blocks);
      this.freeList.push(bd);
    }
  }

  preserveMegaGroups(bds, heapPoolBdescrs) {
    this.freeList = [];
    const sorted_bds = Array.from([...bds].concat([...heapPoolBdescrs])).sort((bd0, bd1) => bd0 - bd1);
    sorted_bds.push(Memory.tagData(rtsConstants.mblock_size * this.capacity) + rtsConstants.offset_first_bdescr);

    this.freeSegment(
        Memory.tagData(rtsConstants.mblock_size * this.staticMBlocks),
        sorted_bds[0] - rtsConstants.offset_first_bdescr);

    for (let i = 0; i < (sorted_bds.length-1); ++i) {
      const l_start = Number(
          this.memory.i64Load(sorted_bds[i] + rtsConstants.offset_bdescr_start)),
      l_blocks =
          this.memory.i32Load(sorted_bds[i] + rtsConstants.offset_bdescr_blocks);
      let l_end = l_start + (rtsConstants.block_size * l_blocks);

      // IVNARIANT: size of a heap pool is always a megablock, so we can
      // skip one megablock to reach the end of the pool
      if (heapPoolBdescrs.has(sorted_bds[i])) {
        l_end = l_start - rtsConstants.offset_first_bdescr + rtsConstants.mblock_size;
      }

      const r = sorted_bds[i + 1] - rtsConstants.offset_first_bdescr;
      this.freeSegment(l_end, r);
    }
    this.freeList.sort(
        (bd0, bd1) => this.memory.i32Load(bd0 + rtsConstants.offset_bdescr_blocks) -
                  this.memory.i32Load(bd1 + rtsConstants.offset_bdescr_blocks));
  }
}
