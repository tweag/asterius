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
      console.log(`checking that bd is aligned... bd: ${bd} | bd - offset: ${bd - rtsConstants.offset_first_bdescr}`);
      this.assertAlignment(bd - rtsConstants.offset_first_bdescr, rtsConstants.mblock_size);
		  const blocks = this.memory.i32Load(bd + rtsConstants.offset_bdescr_blocks);
		  console.log(`using bd from freelist: i:${i} bd:${bd}`);

		  if (alloc_blocks < blocks) {
			  this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, alloc_blocks);
        const rest_base = this.align(bd - rtsConstants.offset_first_bdescr + (rtsConstants.mblock_size * n),
          rtsConstants.mblock_size);

        const rest_bd = rest_base + rtsConstants.offset_first_bdescr;
			  const rest_start = rest_base +
				  rtsConstants.offset_first_block;

			  const rest_end = bd - rtsConstants.offset_first_bdescr + rtsConstants.offset_first_block + blocks * rtsConstants.block_size;
              console.log(`rest_start: ${rest_start} | rest_end: ${rest_end}`);

			//   const rest_nblocks = blocks - alloc_blocks -
			// 	  ((rtsConstants.mblock_size / rtsConstants.block_size) -
			// 		  rtsConstants.blocks_per_mblock);
              const rest_nblocks = Math.floor((rest_end - rest_start) / rtsConstants.block_size);
              
			  if (rest_nblocks <= 0) {
            continue; 
				    throw new WebAssembly.RuntimeError(`not enough spce for alignment! rest_nblocks:${rest_nblocks}`);
			  }


			  console.log(`bd: ${bd} | nblocks: ${blocks} | rest_bd: ${rest_bd} | rest_nblocks: ${rest_nblocks} | freelist (before splice): ${this.freeList}`);
			  this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_start,
				  rest_start);
			  this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_free, rest_start);
			  this.memory.i64Store(rest_bd + rtsConstants.offset_bdescr_link, 0);

			  this.memory.i32Store(rest_bd + rtsConstants.offset_bdescr_blocks,
				  rest_nblocks);

			  this.freeList.splice(i, 1, rest_bd);
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
	  const mblock = this.getMBlocks(n);
	  const bd = mblock + rtsConstants.offset_first_bdescr;
	  const block_addr = mblock + rtsConstants.offset_first_block;
	  this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
	  this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
	  this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
	  this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, alloc_blocks);
	  return bd;
  }

  align(num, align) {
      if ((align & (align - 1)) != 0) {
          throw new WebAssembly.RuntimeError(`${align} is not POT`);
      }
    const x =  ((BigInt(num) + (BigInt(align - 1))) & ~BigInt(align - 1));
    if (!(x >= num)) {
        throw new WebAssembly.RuntimeError(`${x} is not >= ${num}`);
    }
    const x2 = Number(x);
    if (!(x2 >= num)) {
        throw new WebAssembly.RuntimeError(`${x2} = Number(${x}) is not >= ${num}`);
    }

    return x2;
  }

  assertAlignment(num, alignment) {
    const numaligned = this.align(num, alignment);
    if (num != numaligned) {
      throw new WebAssembly.RuntimeError(`not aligned: ${num} | aligned: ${numaligned}`);
    } 
  }

  freeSegment(l_end, r) {
    if (l_end < r) {
      this.memory.memset(l_end, 0, r - l_end);

      const base = this.align(l_end, rtsConstants.mblock_size);
      if(!(base >= l_end && base < r)) {
          throw new WebAssembly.RuntimeError(`base ${base} is encroaching nonfree memory ${l_end} -- ${r}`);
      }


      if (!((BigInt(base) >> BigInt(Math.log2(rtsConstants.mblock_size))) <<
                BigInt(Math.log2(rtsConstants.mblock_size)) == base)) {
          throw new WebAssembly.RuntimeError(`base ${base} is not aligned to ${rtsConstants.mblock_size} -- log2: ${Math.log2(rtsConstants.mblock_size)}`);
      }

      const bd = base + rtsConstants.offset_first_bdescr;
      const start = base + rtsConstants.offset_first_block;
      // const blocks = Math.floor((r - start) / rtsConstants.block_size);
      const blocks = Math.floor((r - start) / (2 * rtsConstants.block_size));
      if (blocks < 0) {
          throw new WebAssembly.RuntimeError(`blocks:${blocks} < 0`);
      }

      this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, start);
      this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, start);
      this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
      this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, blocks);
      console.log(`freeSegment: adding new bd to freelist. bd: ${bd} blocks: ${blocks}`);
      this.freeList.push(bd);
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
