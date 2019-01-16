import * as settings from "./rts.settings.mjs";
import { Memory } from "./rts.memory.mjs";

export class MBlockAlloc {
  constructor(memory) {
    this.memory = memory;
    this.capacity = memory.buffer.byteLength >> Math.log2(settings.mblock_size);
    this.size = this.capacity;
    Object.seal(this);
  }

  getMBlocks(n) {
    if (this.size + n > this.capacity) {
      const d = Math.max(n, this.capacity);
      this.memory.grow(d << Math.log2(settings.mblock_size / settings.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    return Memory.tagData(prev_size << Math.log2(settings.mblock_size));
  }
}
