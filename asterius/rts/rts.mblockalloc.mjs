import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

export class MBlockAlloc {
  constructor() {
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.capacity = undefined;
    this.size = undefined;
    this.bitset = undefined;
    Object.seal(this);
  }

  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.capacity = this.memory.buffer.byteLength / rtsConstants.mblock_size;
    this.size = this.capacity;
    this.bitset = (BigInt(1) << BigInt(this.size)) - BigInt(1);
  }

  getMBlocks(n) {
    if (this.size + n > this.capacity) {
      const d = Math.max(n, this.capacity);
      this.memory.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    this.bitset |= ((BigInt(1) << BigInt(n)) - BigInt(1)) << BigInt(prev_size);
    return Memory.tagData(prev_size * rtsConstants.mblock_size);
  }

  free(p, n) {
    const mblock_no =
      BigInt(Memory.unTag(p)) >> BigInt(Math.log2(rtsConstants.mblock_size));
    this.bitset &= ~(((BigInt(1) << BigInt(n)) - BigInt(1)) << mblock_no);
  }
}
