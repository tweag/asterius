import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";
import * as assert from "assert"

export class MBlockAlloc {
  constructor() {
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.capacity = undefined;
    this.size = undefined;
    this.freeList = [];
    this.ncalls = 0;
    this.all_bds = new Set();
    Object.seal(this);
  }

  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.capacity = this.memory.buffer.byteLength / rtsConstants.mblock_size;
    this.size = this.capacity;
  }

  getMBlocks(n) {
    assert.equal(n > 0, true);
    if (this.size + n > this.capacity) {
      const d = Math.max(n, this.capacity);
      this.memory.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    let mblock = Memory.tagData(prev_size * rtsConstants.mblock_size);
    this.all_bds.add(mblock + rtsConstants.offset_first_bdescr);
    return mblock;
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
        this.all_bds.add(bd);
        return bd;
      }
      if (req_blocks == blocks) {
        this.freeList.splice(i, 1);
        this.all_bds.add(bd);
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
    this.all_bds.add(bd);
    return bd;
  }

  freeSegment(l_end, r) {
    if (l_end < r) {
      console.log(`segments.append([${l_end}, ${r}, "free"])`)
      this.memory.memset(l_end, 0x42, r - l_end);
      const bd = l_end + rtsConstants.offset_first_bdescr,
            start = l_end + rtsConstants.offset_first_block,
            blocks = (r - start) / rtsConstants.block_size;
      this.freeList.push(bd);
    }
  }

  preserveMegaGroups(bds, heapPools) {
    console.log(`### preserveMegaGroups: ${this.ncalls++} ###`)
    console.log(`segments = []`);


    for(let i = 0; i < heapPools.length; ++i) {
      const bd = heapPools[i];
      const l = Number(this.memory.i64Load(bd + rtsConstants.offset_bdescr_start));
      const blocks = this.memory.i32Load(bd + rtsConstants.offset_bdescr_blocks);
      const r = l + blocks * rtsConstants.block_size;

      // add the heap pool into the used block descriptors list.
      bds.add(bd);
      console.log(`segments.append([${l}, ${r}, "heappool"])`);
    }

    const all_bds = [...this.all_bds];
    for(let i = 0; i < all_bds.length; ++i) {
      let bd = all_bds[i];
      const l_start = Number(
          this.memory.i64Load(bd + rtsConstants.offset_bdescr_start)),
      l_blocks =
          this.memory.i32Load(bd + rtsConstants.offset_bdescr_blocks),
      l_end = l_start + (rtsConstants.block_size * l_blocks);
      if (bds.has(bd)) {
        console.log(`segments.append([${l_start}, ${l_end}, "live"])`);
      } else  {
        console.log(`segments.append([${l_start}, ${l_end}, "dead"])`);
      }
    }


    this.freeList = [];
    const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
    const heapPoolsSet = new Set([...heapPools]);

    this.freeSegment(
        Memory.tagData(rtsConstants.mblock_size * this.staticMBlocks),
        sorted_bds[0] - rtsConstants.offset_first_bdescr);


    console.log(`# all_bds: [${[...this.all_bds]}] | sorted_bds: [${sorted_bds}] | heapPools: [${heapPools}]`);
    for (let i = 0; i < (sorted_bds.length-1); ++i) {
      const bd = sorted_bds[i];
      const bd_next = sorted_bds[i+1];

      const l_start = Number(
          this.memory.i64Load(bd + rtsConstants.offset_bdescr_start));
      const l_blocks =
          this.memory.i32Load(bd + rtsConstants.offset_bdescr_blocks);
      let l_end = l_start + (rtsConstants.block_size * l_blocks);

      const flags = this.memory.i16Load(bd + rtsConstants.offset_bdescr_flags);
      // if we are in a heap region, then just don't touch it
      // similarly, if we are in a pinned region, don't touch it
      if (heapPoolsSet.has(bd) || flags & rtsConstants.BF_PINNED) {
        continue;
        // l_end = l_start - rtsConstants.offset_bdescr_start + rtsConstants.mblock_size;
      }

      const r = bd_next - rtsConstants.offset_first_bdescr;
      this.freeSegment(l_end, r);
    }

    this.freeList.sort(
        (bd0, bd1) => this.memory.i32Load(bd0 + rtsConstants.offset_bdescr_blocks) -
                  this.memory.i32Load(bd1 + rtsConstants.offset_bdescr_blocks));

    // console.log(`draw_segments(segments)`);
    console.log(`####### run ipython -i draw.py and copy-paste code in between #####`);
  }
}
