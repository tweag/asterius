// Block allocator. This hands out blocks to the heap, and internally uses
// the megablock allocator to allocate memory. For more information, see:
// https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/block-alloc
// https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/BlockAlloc.c
// https://gitlab.haskell.org/ghc/ghc/blob/master/includes/rts/storage/Block.h
import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";
import * as assert from "assert";

function ptr2bdescr(ptr) {
    throw new WebAssembly.RuntimeError("unimplemented");
}
// check that a megablock is correctly aligned
function assertMblockAligned(mb) {
    const n = Math.log2(rtsConstants.mblock_size);
    assert.equal((BigInt(mb) >> BigInt(n)) << BigInt(n), BigInt(mb));
}

// check that a block descriptor is correctly aligned
function assertBdescrAligned(bd) {
    assertMblockAligned(bd - rtsConstants.offset_first_bdescr);
}

export class BlockAlloc {
  constructor() {
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.capacity = undefined;
    this.size = undefined;
    this.bdescr2realbdescr = new Map();
    // this.freeList = [];
    this.freeSegments = [];
    this.freeMegablocks = [];
    Object.seal(this);
  }

  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.capacity = this.memory.buffer.byteLength / rtsConstants.mblock_size;
    this.size = this.capacity;
  }

  
  getBlocks__(n) {
    if (this.size + n > this.capacity) {
      const d = Math.max(n, this.capacity);
      this.memory.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    return Memory.tagData(prev_size * rtsConstants.mblock_size);
  }

  allocBlocks(n) {
    const req_blocks = n;
    const req_mblocks = Math.min(1, Math.ceil(n / rtsConstants.mblock_size));

    // look for free megablocks
    
    for(let i = 0; i < this.freeMegablocks.length; ++i) {
        const [mblock, block_addr, nblocks] = this.freeMegablocks[i];
        // this megablock has enough space.
        if (nblocks <= req_blocks) {
            console.log("Allocating from freeMegaBlocks");
            // get the pointer to block descriptor
            let bdescr = ptr2bdescr(ptr);
            this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
            this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
            this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
            this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
    
            // if we have leftover blocks, then add back the megablock to the freelist.
            if (nblocks < req_blocks) {
                this.freeMegablocks.splice(i, [mblock, block_addr_next, nblocks_next]);

                // get the next free pointer.
                let block_addr_next = block_addr + rtsConstants.sizeof_block * req_blocks;
                nblocks_next = nblocks - req_blocks;
    
            } else {
                assert.equal(nblocks, req_blocks);
                this.freeMegablocks.splice(i);
            }
            return block_addr;

        }
    }
    

    // we don't have free megablocks to pull blocks from, so allocate them.
    const mblock = this.getBlocks__(n);
    const bd = mblock + rtsConstants.offset_first_bdescr;
    const block_addr = mblock + rtsConstants.offset_first_block;
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
    this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);

    // if the whole thing is still happening inside a single megablock, then we still have space.
    if (req_mblocks <= 1) { 
        console.log("requested less than 1 megablock");
        const block_addr = mblock + rtsConstants.offset_first_block;
        // address of the next block
        const block_addr_next = block_addr + rtsConstants.block_size * n;
        const nblocks_next = rtsConstants.blocks_per_mblock - req_blocks; 
        this.freeMegablocks.push([mblock,block_addr_next,  nblocks_next]);
    } else {
        console.log(`requested megablocks: ${req_mblocks}`);1
    }
    return bd;


    /*
    for (let i = 0; i < this.freeSegments.length; ++i) {
      const [bd, r] = this.freeSegments[i];
      assertBdescrAligned(bd);
      const mblock = bd - rtsConstants.offset_first_bdescr;
      assertMblockAligned(mblock);
      const start = mblock + rtsConstants.offset_first_block;
      const blocks = Math.floor((r - start) / rtsConstants.block_size);
    
      if (req_blocks < blocks) {
        // console.log("allocating from free segment");
        this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
        const rest_bd = bd + (rtsConstants.mblock_size * n);
        assertBdescrAligned(rest_bd);
        const rest_start = rest_bd - rtsConstants.offset_first_bdescr + rtsConstants.offset_first_block;
        const rest_blocks = Math.floor((r - rest_start) / rtsConstants.block_size);
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_start, rest_start);
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_free, rest_start);
        this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_link, 0);
        // 0xDEADBEEF = 3735928559
        this.memory.i32Store(rest_bd + rtsConstants.offset_bdescr_blocks, rest_blocks);
        this.freeSegments.splice(i, 1, [rest_bd, r]);
        // this.freeSegments.splice(i, 1);

        
        // this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
        // const rest_bd = bd + (rtsConstants.mblock_size * n),
        //       rest_start = rest_bd - rtsConstants.offset_first_bdescr +
        //                    rtsConstants.offset_first_block;
        // this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_start,
        //                      rest_start);
        // this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_free, rest_start);
        // this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_link, 0);
        // this.memory.i32Store(rest_bd + rtsConstants.offset_bdescr_blocks,
        //                      blocks - req_blocks -
        //                          ((rtsConstants.mblock_size / rtsConstants.block_size) -
        //                           rtsConstants.blocks_per_mblock));
        // this.freeList.splice(i, 1, rest_bd);
        return bd;
      }

      if (req_blocks == blocks) {
        // console.log("allocating from free segment");
        this.freeSegments.splice(i, 1);
        return bd;
      }
     
    }
     */
    // console.log("allocating from new segment");
    /*
    */

    // what we need fits inside a megablock.
    if (req_mblocks <= 1) {
        throw new WebAssembly.RuntimeError(`have not implemented how to allocate into a single megablock: ${req_mblocks}`);
    } else {
        
        throw new WebAssembly.RuntimeError(`have not implemented how to allocate into multiple megablocks: ${req_mblocks}`);
    }
  }

  freeSegment(i, l_end, r) {
    if (l_end < r) {
        this.memory.memset(l_end, 0x42 + i, r - l_end);
        // const bd = l_end + rtsConstants.offset_first_bdescr;
        // this.freeSegments.push([l_end, r]);
        // this.freeSegments.push([bd, r])
    }
  }

  preserveGroups(bds) {
    this.freeSegments = [];
    const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
    // sorted_bds.push(Memory.tagData(rtsConstants.mblock_size * this.capacity) + rtsConstants.offset_first_bdescr);
    this.freeSegment(0, 
        Memory.tagData(rtsConstants.mblock_size * this.staticMBlocks),
        sorted_bds[0] - rtsConstants.offset_first_bdescr);
    for (let i = 0; i < (sorted_bds.length-1); ++i) {
      const l_start = Number(
          this.memory.i64Load(sorted_bds[i] + rtsConstants.offset_bdescr_start)),
      l_blocks =
          this.memory.i32Load(sorted_bds[i] + rtsConstants.offset_bdescr_blocks),
      l_end = l_start + (rtsConstants.block_size * l_blocks),
      r = sorted_bds[i + 1] - rtsConstants.offset_first_bdescr;
      this.freeSegment(i, l_end, r);
    }
  }
}
