import * as settings from "./rts.settings.mjs";
import { Memory } from "./rts.memory.mjs";

export class MBlockAlloc {
  constructor(memory) {
    this.memory = memory;
    this.capacity = undefined;
    this.size = undefined;
    this.freeList = [];
    Object.seal(this);
  }

  init() {
    this.capacity =
        this.memory.buffer.byteLength >> Math.log2(settings.mblock_size);
    this.size = this.capacity;
  }

  getMBlocks(n) {
    if (this.size + n > this.capacity) {
      const d = Math.max(n, this.capacity);
      this.memory.grow(
          d << Math.log2(settings.mblock_size / settings.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    return Memory.tagData(prev_size << Math.log2(settings.mblock_size));
  }

  allocMegaGroup(n) {
    for (let i = 0; i < this.freeList.length; ++i) {
      const bd = this.freeList[i];
      if (this.memory.i32Load(bd + settings.offset_bdescr_blocks) >= n) {
        this.freeList.splice(i, 1);
        return bd;
      }
    }
    const mblock = this.getMBlocks(n),
          bd = mblock + settings.offset_first_bdescr,
          block_addr = mblock + settings.offset_first_block;
    this.memory.i64Store(bd + settings.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + settings.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + settings.offset_bdescr_link, 0);
    this.memory.i32Store(
        bd + settings.offset_bdescr_blocks,
        n == 1 ? settings.blocks_per_mblock
               : settings.blocks_per_mblock +
                     (settings.mblock_size / settings.block_size) * (n - 1));
    return bd;
  }

  freeMegaGroup(bd) {
    this.memory.i64Store(
        bd + settings.offset_bdescr_free,
        this.memory.i64Load(bd + settings.offset_bdescr_start));
    this.freeList.push(bd);
  }
}
