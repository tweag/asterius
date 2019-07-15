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
    this.freeMegablocks = [];
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
    const req_mblocks = Math.min(1, Math.ceil(req_blocks / rtsConstants.mblock_size));
    console.log(`req_blocks: ${req_blocks} | req_mblocks: ${req_mblocks}`);

    for(let i = 0; i < this.freeMegablocks.length; ++i) {
      const [mblock, block_addr, nblocks] = this.freeMegablocks[i];
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
          const nblocks_next = nblocks - req_blocks;
          // this.freeMegablocks.splice(i, [mblock, block_addr_next, nblocks_next]);
          this.freeMegablocks.splice(i);

        } else {
          assert.equal(nblocks, req_blocks);
          this.freeMegablocks.splice(i);
        }
        console.log("returning: ${block_addr}");
        return bd;

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
      this.freeMegablocks.push([mblock,block_addr_next,  nblocks_next]);
    } else {
      console.log(`requested megablocks: ${req_mblocks}`);1
    }
    console.log(`returning: ${bd}`);
    return bd;
  }

  freeSegment(i, l_end, r) {
    // return;
    if (l_end < r) {
      this.memory.memset(l_end, 0x42 + i, r - l_end);
      // const bd = l_end + rtsConstants.offset_first_bdescr;
    }
  }

  preserveGroups(bds) {
    return; 
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
