// Block allocator. This hands out blocks to the heap, and internally uses
// the megablock allocator to allocate memory. For more information, see:
// https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/block-alloc
// https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/BlockAlloc.c
// https://gitlab.haskell.org/ghc/ghc/blob/master/includes/rts/storage/Block.h
import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";
import * as assert from "assert";

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
    
    const req_mblocks = 
          Math.ceil(((rtsConstants.mblock_size * n) - rtsConstants.offset_first_block) / 
          rtsConstants.block_size);

    const req_blocks = 
          Math.ceil(((rtsConstants.mblock_size * n) - rtsConstants.offset_first_block) / 
          rtsConstants.block_size);

    // what we need fits inside a megablock.
    if (req_mblocks == 1) {

    } else {
        throw new WebAssembly.RuntimeError("have not implemented how to allocate into multiple megablocks");
    }
  }

  freeSegment(l_end, r) {
    if (l_end < r) {
        this.memory.memset(l_end, 0, r - l_end);
        const bd = l_end + rtsConstants.offset_first_bdescr;
        assertBdescrAligned(bd);
        const start = l_end + rtsConstants.offset_first_block;
        const blocks = Math.floor((r - start) / rtsConstants.block_size);
        this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, start);
        this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, start);
        this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
        this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, blocks);
        this.freeSegments.push([bd, r])
    }
  }

  preserveMegaGroups(bds) {
      return; 
    // this.freeList = [];
    this.freeSegments = [];
    const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
    // sorted_bds.push(Memory.tagData(rtsConstants.mblock_size * this.capacity) + rtsConstants.offset_first_bdescr);
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
  }
}
