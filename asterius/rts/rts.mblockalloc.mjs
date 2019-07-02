import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";
import { fstat, open, close, writeFile, writeFileSync } from "fs";

export class MBlockAlloc {
  constructor() {
    
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.capacity = undefined;
    this.size = undefined;
    this.all_bds = [];
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
    const req_blocks = ((rtsConstants.mblock_size * n) - rtsConstants.offset_first_block) / rtsConstants.block_size;

    /*
    // QUESTION: why do we subtract offset_first_block? isn't this doing a *size* calculation?

    for (let i = 0; i < this.freeList.length; ++i) {
      // blocks = number of blocks in megablock??
      const bd = this.freeList[i];
      const blocks = this.memory.i32Load(bd + rtsConstants.offset_bdescr_blocks);
      
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
    
    const mblock = this.getMBlocks_(n);
    const bd = mblock + rtsConstants.offset_first_bdescr;
    this.all_bds.push(bd);
    const block_addr = mblock + rtsConstants.offset_first_block;
    
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
    this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
    return bd;
  }

  // frees the segment between `l_end` and `r`, COUNTED IN BYTES
  freeSegment(l_end, r, ix) {
    if (l_end < r) {
      // Change the memset to write 0x42 so we can see a clear pattern in the debug log.
      this.memory.memset(l_end, 0x42 + ix, r - l_end);
      // const bd = l_end + rtsConstants.offset_first_bdescr;
      // const start = l_end + rtsConstants.offset_first_block;
      // const blocks = (r - start) / rtsConstants.block_size;
      // 
      // this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, start);
      // this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, start);
      // this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
      // this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, blocks);
      // // push the block descriptor pointer into the freelist
      // this.freeList.push(bd);
    }
  }

  // bds: List of live MegaBlocks
  preserveMegaGroups(bds) {
    console.log("bds: ", bds);
    // sort in ascending order
    const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);

    // add the block descriptor of the final block as well??
    // we don't have this block descriptor on our list, when do we initialize
    // this block descriptor??
    // sorted_bds.push(Memory.tagData(rtsConstants.mblock_size * this.capacity) +
    //                 rtsConstants.offset_first_bdescr);
    
    // this.freeSegment(
    //     Memory.tagData(rtsConstants.mblock_size * this.staticMBlocks),
    //   sorted_bds[0] - rtsConstants.offset_first_bdescr);
    // 
    console.log("sorted_bds: ", sorted_bds);
    console.log("all_bds: ", this.all_bds);

    // do not clear this way. Rather, for each block descriptor we have, clear
    // the free memory in that block descriptor
    for(let i = 0; i < this.all_bds.length; ++i) {
      const l_start = Number(this.memory.i64Load(this.all_bds[i] + rtsConstants.offset_bdescr_start));

      const l_blocks =
      this.memory.i32Load(this.all_bds[i] + rtsConstants.offset_bdescr_blocks);
      const l_end_used = l_start + (rtsConstants.block_size * l_blocks);
      const l_end_total = l_start + rtsConstants.block_size * rtsConstants.blocks_per_mblock;


      let datastr = ""
      for(let ptr = l_start; ptr < l_end_total; ptr += 8) {
        datastr += Number(this.memory.i8Load(ptr).toString(16));

      }
      writeFileSync("/tmp/mblock" + i + ".txt", datastr, (err) => {
        if (err) throw err;
      });


      // if this BD is not live, AND this BD is not the 2nd BD
      // if we allow this BD to live, then we get to the second GC.
      // if (!bds.has(this.all_bds[i]) && this.all_bds[i] != 9007160603181312n) {
      if (!bds.has(this.all_bds[i])) {

        this.freeSegment(l_start, l_end_total, i);

        console.log("->>>> freeSegment: i: ", i,
        "bd: ", this.all_bds[i],
        "\n\tl_start: " , l_start,
        " l_end_total: ", l_end_total,
        "l_end_used: ", l_end_used,
        "l_end_total - l_end_used: ", l_end_total - l_end_used);

      
      }
    }

    /*
    for (let i = 0; i < (sorted_bds.length-1); ++i) {
      
      // ensure that we have actually created the block descriptor
      const ix_in_all = this.all_bds.findIndex(function(v) { return v == sorted_bds[i]});
      if (ix_in_all == -1) {
        throw new WebAssembly.RuntimeError("unable to find block descriptor: " +
                                           sorted_bds[i]);
        
      }
      const ix2_in_all = this.all_bds.findIndex(function(v) { return v == sorted_bds[i+1]});
      if (ix2_in_all == -1) {
        throw new WebAssembly.RuntimeError("unable to find block descriptor2: " +
                                           sorted_bds[i+1] + " i:"  + i);
        
      }

      
      // const l_start = Number( this.memory.i64Load(sorted_bds[i] +
      // rtsConstants.offset_bdescr_start));
      const l_start = Number(this.memory.i64Load(sorted_bds[i] +
                                                 rtsConstants.offset_bdescr_start));
       
      const l_blocks =
            this.memory.i32Load(sorted_bds[i] + rtsConstants.offset_bdescr_blocks);
      const l_end = l_start + (rtsConstants.block_size * l_blocks);
      const r =  l_start + rtsConstants.block_size * rtsConstants.blocks_per_mblock;
      
      const r_prev = sorted_bds[i + 1] - rtsConstants.offset_first_bdescr;
      
      console.log("->>>> freeSegment: i: ", i,
                  "bd: ", sorted_bds[i], " |bd[i+1]: ", sorted_bds[i+1],
                  "\n\tix in all: ", ix_in_all,
                  "\n\tl_start: " , l_start,
                  " l_blocks: ", l_blocks, "l_end; ", 
        l_end, "\n\tr: ", r, " r_prev: ", r_prev, "r_prev - r: ", r_prev - r);
        this.freeSegment(l_end, r_prev, i);
    }
    */


  }
}
