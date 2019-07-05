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
    // number of blocks we can allocate in this megablock.
    console.log(`mblock size: ${rtsConstants.mblock_size} | offset_first_block: ${rtsConstants.offset_first_block} | blockSize: ${rtsConstants.block_size}`);
    const alloc_blocks = Math.floor(((rtsConstants.mblock_size * n) - rtsConstants.offset_first_block) / rtsConstants.block_size);

    for (let i = 0; i < this.freeList.length; ++i) {
      const bd = this.freeList[i];
      const blocks = this.memory.i32Load(bd + rtsConstants.offset_bdescr_blocks);

      if (alloc_blocks < blocks) {
        this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, alloc_blocks);
        const rest_bd = bd + (rtsConstants.mblock_size * n);
        const rest_start = rest_bd - rtsConstants.offset_first_bdescr +
                           rtsConstants.offset_first_block;
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_start,
                             rest_start);
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_free, rest_start);
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_link, 0);

        this.memory.i32Store(rest_bd + rtsConstants.offset_bdescr_blocks,
                             blocks - alloc_blocks -
                                 ((rtsConstants.mblock_size / rtsConstants.block_size) -
                                  rtsConstants.blocks_per_mblock));
        console.log(`bd: ${bd} | rest_bd: ${rest_bd} | freelist (before splice): ${this.freeList}`);
        this.freeList.splice(i, 1, rest_bd);
        this.freeList.splice(i, 1);
        console.log(`bd: ${bd} | rest_bd: ${rest_bd} | freelist (after splice): ${this.freeList}`);
        return bd;
      }
      if (alloc_blocks == blocks) {
        console.log(`bd: ${bd} | rest_bd: NONE | freelist (before splice): ${this.freeList}`);
        this.freeList.splice(i, 1);
        console.log(`bd: ${bd} | rest_bd: NONE | freelist (after splice): ${this.freeList}`);
        return bd;
      }
    }

      // test the freeList code without involving the freeSegment code. So this
      // appears to pass.
    for(let i = 0; i < 2; ++i) {
        const mblock = this.getMBlocks(n);
        const bd = mblock + rtsConstants.offset_first_bdescr;
        const block_addr = mblock + rtsConstants.offset_first_block;
        this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
        this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
        this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
        this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, alloc_blocks);
        if (i == 0) {
            this.freeList.push(bd);
        } else {
            return bd;
        }
    }
  }

  freeSegment(l_end, r) {
    if (l_end < r) {
      this.memory.memset(l_end, 0, r - l_end);
      const bd = l_end + rtsConstants.offset_first_bdescr,
            start = l_end + rtsConstants.offset_first_block,
            blocks = (r - start) / rtsConstants.block_size;
      this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, start);
      this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, start);
      this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
      this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, blocks);

      // this.freeList.push(bd);
    }
  }

  preserveMegaGroups(bds) {
    this.freeList = [];
    const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
    sorted_bds.push(Memory.tagData(rtsConstants.mblock_size * this.capacity) + rtsConstants.offset_first_bdescr);
    this.freeSegment(
        Memory.tagData(rtsConstants.mblock_size * this.staticMBlocks),
        sorted_bds[0] - rtsConstants.offset_first_bdescr);
    for (let i = 0; i < (sorted_bds.length-1); ++i) {
      const l_start = Number(
          this.memory.i64Load(sorted_bds[i] + rtsConstants.offset_bdescr_start)),
      l_blocks =
          this.memory.i32Load(sorted_bds[i] + rtsConstants.offset_bdescr_blocks),
      l_end = l_start + (rtsConstants.block_size * l_blocks),
      r = sorted_bds[i + 1] - rtsConstants.offset_first_bdescr;
      this.freeSegment(l_end, r);
    }
    this.freeList.sort(
        (bd0, bd1) => this.memory.i32Load(bd0 + rtsConstants.offset_bdescr_blocks) -
                  this.memory.i32Load(bd1 + rtsConstants.offset_bdescr_blocks));
  }
}
