import * as settings from "./rts.settings.mjs";
import { Memory } from "./rts.memory.mjs";

export class MBlockAlloc {
  constructor(memory) {
    this.memory = memory;
    this.capacity = null;
    this.size = null;
    Object.seal(this);
  }

  init() {
    this.capacity = this.memory.buffer.byteLength >> Math.log2(settings.mblock_size);
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
    const mblock = this.getMBlocks(n),
          bd = mblock + settings.offset_first_bdescr,
          block_addr = mblock + settings.offset_first_block;
    this.memory.i64Store(bd + settings.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + settings.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + settings.offset_bdescr_link, 0);
    this.memory.i32Store(
        bd + settings.offset_bdescr_blocks,
        n === 1 ? settings.blocks_per_mblock
                : settings.blocks_per_mblock +
                       (settings.mblock_size / settings.block_size) * (n - 1));
    return bd;
  }
}
