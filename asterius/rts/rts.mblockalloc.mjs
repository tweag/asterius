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
    // capacity = number of mblocks available.
    this.capacity = this.memory.buffer.byteLength / rtsConstants.mblock_size;
    this.size = this.capacity;
  }


  // return the pointer pointing to N new megablocks.
  getMBlocks_(n) {
    if (this.size + n > this.capacity) {
      // d = number of blocks to add. Use doubling to ensure that
      // amortized cost is O(1).
      const d = Math.max(n, this.capacity);
      
      // NOTE: memory.grow takes number of pages.
      this.memory.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    return Memory.tagData(prev_size * rtsConstants.mblock_size);
  }

  // allocate n megablocks contiguously.
  allocMegaGroup(n) {
    // QUESTION: why do we subtract offset_first_block? isn't this doing a *size* calculation?
    const req_blocks = ((rtsConstants.mblock_size * n) - rtsConstants.offset_first_block) / rtsConstants.block_size;


    for (let i = 0; i < this.freeList.length; ++i) {
      // blocks = number of blocks in megablock??
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

      // else??
      throw new WebAssembly.RuntimeError("should never reach here, we should always be able to allocate enough blocks");
    }
    const mblock = this.getMBlocks_(n),
          bd = mblock + rtsConstants.offset_first_bdescr,
          block_addr = mblock + rtsConstants.offset_first_block;
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
    this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
    return bd;
  }

  // frees the segment between `l_end` and `r`, COUNTED IN BYTES
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
      // push the block descriptor pointer into the freelist
      this.freeList.push(bd);
    }
  }

  // bds: List of live MegaBlocks
  preserveMegaGroups(bds) {
    this.freeList = [];
    console.log("bds: ", bds);
    // sort in ascending order
    const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
    console.log("sorted_bds: ", sorted_bds);

    // add the block descriptor of the final block as well??
    sorted_bds.push(Memory.tagData(rtsConstants.mblock_size * this.capacity) +
                    rtsConstants.offset_first_bdescr);
    
    // this.freeSegment(
    //     Memory.tagData(rtsConstants.mblock_size * this.staticMBlocks),
    //   sorted_bds[0] - rtsConstants.offset_first_bdescr);
    // 
    for (let i = 0; i < (sorted_bds.length-1); ++i) {
      console.log("hallo")
      // const l_start = Number( this.memory.i64Load(sorted_bds[i] + rtsConstants.offset_bdescr_start));
      const l_start = Number(this.memory.i64Load(sorted_bds[i] + rtsConstants.offset_bdescr_start));
       
      const l_blocks =
          this.memory.i32Load(sorted_bds[i] + rtsConstants.offset_bdescr_blocks);
      const l_end = l_start + (rtsConstants.block_size * l_blocks);
      const r =  l_start + rtsConstants.block_size * rtsConstants.blocks_per_mblock;  
      const r_prev = sorted_bds[i + 1] - rtsConstants.offset_first_bdescr;
      
      // if (i == 0) { continue; }
      console.log("->>>> freeSegment: i: ", i, " l_start: " , l_start, " l_blocks: ", l_blocks, "l_end; ", 
        l_end, " r: ", r, " r_prev: ", r_prev, "r_prev - r: ", r_prev - r);
      this.freeSegment(l_end, r);
    }

    // sort the freeList by number of blocks available.
    this.freeList.sort(
        (bd0, bd1) => this.memory.i32Load(bd0 + rtsConstants.offset_bdescr_blocks) -
                  this.memory.i32Load(bd1 + rtsConstants.offset_bdescr_blocks));
  }
}
