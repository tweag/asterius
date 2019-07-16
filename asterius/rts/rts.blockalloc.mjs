// Block allocator. This hands out blocks to the heap, and internally uses
// the megablock allocator to allocate memory. For more information, see:
// https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/block-alloc
// https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/BlockAlloc.c
// https://gitlab.haskell.org/ghc/ghc/blob/master/includes/rts/storage/Block.h
//
//
// The algorithm described below is entirely from:
// - BlockAlloc.c:allocGroup
// - BlockAlloc.c:allocGroupOnNode
// When we are asked for `n` blocks, here's the allocation strategy:
// - IF the blocks DO NOT fit into a single megablock: 
//     Allocate a new megagroup (a group of megablocks), setup the
//     block descriptor and move on with life.
// - OTHERWISE, the blocks DO fit in a single megablock:
//     Look in the megablock freelist. If we find a megablock that
//     can fit our blocks, allocate. If there are leftover blocks,
//     add them back.
//
import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";
import * as assert from "assert";

function ptr2bdescr(p) {
    // #define BLOCK_MASK (BLOCK_SIZE-1)
    // #define MBLOCK_MASK (MBLOCK_SIZE-1)
    // #define Bdescr(p) \
    // ((((p) &  MBLOCK_MASK & ~BLOCK_MASK) >> (BLOCK_SHIFT-BDESCR_SHIFT)) | ((p) & ~MBLOCK_MASK))
    const BLOCK_MASK = BigInt(rtsConstants.block_size - 1);
    const MBLOCK_MASK = BigInt(rtsConstants.mblock_size - 1);
    const BLOCK_SHIFT = BigInt(rtsConstants.block_shift);
    const BDESCR_SHIFT = BigInt(rtsConstants.bdescr_shift);

    // console.log(`BLOCK_MASK: ${BLOCK_MASK} | MBLOCK_MASK: ${MBLOCK_MASK} | BLOCK_SHIFT: ${BLOCK_SHIFT} | BDESCR_SHIFT: ${BDESCR_SHIFT}`);
    p = BigInt(p);
    return Number((((p) &  MBLOCK_MASK & ~BLOCK_MASK) >> (BLOCK_SHIFT-BDESCR_SHIFT)) | ((p) & ~MBLOCK_MASK));
}

// return the megablock a block descriptor belongs to
function bdescr2megablock(bd) {
  const MBLOCK_MASK = BigInt(rtsConstants.mblock_size - 1);
  return Number(BigInt(bd) & MBLOCK_MASK);
}

// get the count of bdescr in the megablock.
// 0 if it is the first bdescr
// 1 if it is the second bdescr
// etc
function bdescr2index(bd) {
  const base = bdescr2megablock(bd);
  assert.equal((bd - base) % rtsConstants.sizeof_bdescr, 0);
  return (bd - base) / rtsConstants.sizeof_bdescr;
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
    this.freeBlocks = [];
    Object.seal(this);
  }

  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.capacity = this.memory.buffer.byteLength / rtsConstants.mblock_size;
    this.size = this.capacity;
  }


  allocMegaBlocks(n) {
    if (this.size + n > this.capacity) {
      const d = Math.max(n, this.capacity);
      this.memory.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    return Memory.tagData(prev_size * rtsConstants.mblock_size);
  }


  allocBlocks(req_blocks) {
    const req_mblocks = Math.ceil(req_blocks / rtsConstants.mblock_size);
    console.log(`req_blocks: ${req_blocks} | req_mblocks: ${req_mblocks}`);

    // we want to allocate something smaller than a megablock
    if (req_mblocks == 0) {
      // look through our free block list
      for(let i = 0; i < this.freeBlocks.length; ++i) {
        const [mblock, l_end, r] = this.freeBlocks[i];
        const blocks = (r - l_end) / rtsConstants.block_size;
        if (req_blocks <= nblocks) {
          console.log(`allocBlocks: req_blocks: ${req_blocks} | i:${i} | mblock: ${mblock} | bdescr: ${ptr2bdescr(block_addr)} | nblocks: ${nblocks}`);
          // initialize the block descriptor.
          const bd = ptr2bdescr(block_addr);
          this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
          this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
          this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
          this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);

          // if we have leftover blocks, then add back the megablock to the freelist.
          if (req_blocks < nblocks) {
            // get the next free pointer.
            console.log(`sizeof_block: ${rtsConstants.block_size}`);
            const block_addr_next = block_addr + rtsConstants.block_size * (1 + req_blocks);
            console.log(`block_addr_next: ${block_addr_next} | block_size: ${rtsConstants.block_size}`);
            // check that the block descriptor of the new block is not the same
            // as our block descriptor. Indeed, it should be "ahead".

            if (ptr2bdescr(block_addr_next) <= bd) {
              throw new WebAssembly.RuntimeError("we are getting the same bd, not the next bd from the new block");
            }
            const nblocks_next = nblocks - (req_blocks + 2);
            // this.freeBlocks.splice(i, [mblock, block_addr_next, nblocks_next]);
            this.freeBlocks.splice(i);

          } else {
            assert.equal(nblocks, req_blocks);
            this.freeBlocks.splice(i);
          }
          console.log(`returning: ${bd}`);
          return bd;

        }
      }
    }

    // we don't have free megablocks to pull blocks from, so allocate them.
    const mblock = this.allocMegaBlocks(req_mblocks);
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
      const block_addr_next = block_addr + rtsConstants.block_size * req_blocks;
      const nblocks_next = rtsConstants.blocks_per_mblock - req_blocks; 
      this.freeBlocks.push([mblock,block_addr_next,  nblocks_next]);
    } else {
      console.log(`requested megablocks: ${req_mblocks}`);1
    }
    console.log(`returning: ${bd}`);
    return bd;
  }

  freeSegment(i, mblock, l_end, r) {
    if (l_end < r) {
      this.memory.memset(l_end, 0x42 + i, r - l_end);
      this.freeBlocks.push([mblock, l_end, r]);
    }
  }

  preserveGroups(bds) {
    console.log("preserveGroups:");
    console.log(`bds: ${Array.from(bds)}`);
    bds = Array.from(bds);
    // const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
    // this.freeSegment(0, 
    //   Memory.tagData(rtsConstants.mblock_size * this.staticMBlocks),
    //   sorted_bds[0] - rtsConstants.offset_first_bdescr);

    var m = new Map();
    for(var i = 0; i < bds.length; ++i) {
      console.log(`--- ${i}: bds[${i}] = ${bds[i]}`);
      const mblock =  bdescr2megablock(bds[i]);
      if(m.has(mblock)) {
        m[mblock].push(bds[i]);
      } else {
        m[mblock] = [bds[i]];
      }
    }
    
    // current index passed to freeSegment
    let ix = 0;

    let sorted_megablocks = Array.from(m.keys()).sort((m0, m1) => m0 - m1);
    
    // we are allowed to free between blocks.
    // we are allowed to take up an entire megablock if the megablock is free.
    for(var i = 0; i < sorted_megablocks.length; ++i) {
      const mblock = sorted_megablocks[i];
      const bds = m[mblock];
      const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
      for(var j = 0; j < sorted_bds.length; ++j) {
        const l = this.memory.i64Load(sorted_bds[j] + rtsConstants.offset_bdescr_start);

        if (j < sorted_bds.length - 1) {
          // find the ending location of the block pointed to to by sorted_bds[j+1]
          const r = bdescr2megablock(sorted_bds[j+1]) + rtsConstants.offset_first_block +  bdescr2index(sorted_bds[j+1]) * rtsConstants.block_size;
        }
        else if (i < sorted_megablocks.length - 1) {
          // if we are at the end of the current megablock, we can wipe everything till the next
          // megablock.
          const r = sorted_megablocks[i+1];
        }
        throw new WebAssembly.RuntimeError("foo");
        this.freeSegment(ix++, mblock, l, r);
      }
    }
  }
}
