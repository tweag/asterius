import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

function mask(n) {
  return (BigInt(1) << BigInt(n)) - BigInt(1);
}

export class MBlockAlloc {
  constructor() {
    this.memory = undefined;
    this.capacity = undefined;
    this.bitset = undefined;
    Object.seal(this);
  }

  init(memory) {
    this.memory = memory;
    this.capacity = this.memory.buffer.byteLength / rtsConstants.mblock_size;
    this.bitset = mask(this.capacity);
  }

  getMBlocks(n) {
    const m = mask(n);
    for (let i = BigInt(0); i <= BigInt(this.capacity - n); ++i) {
      const mi = m << i;
      if (!(this.bitset & mi)) {
        this.bitset |= mi;
        return Memory.tagData(Number(i) * rtsConstants.mblock_size);
      }
    }
    const d = Math.max(n, this.capacity),
      prev_capacity = this.capacity;
    this.memory.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
    this.capacity += d;
    this.bitset |= m << BigInt(prev_capacity);
    return Memory.tagData(prev_capacity * rtsConstants.mblock_size);
  }

  free(p, n) {
    const mblock_no =
      BigInt(Memory.unTag(p)) >> BigInt(Math.log2(rtsConstants.mblock_size));
    this.bitset &= ~(mask(n) << mblock_no);
  }
}
