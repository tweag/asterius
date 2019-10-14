import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

function mask(n) {
  return (BigInt(1) << BigInt(n)) - BigInt(1);
}

/**
 * MBlock manager.
 *
 * @property capacity Number of allocated MBlocks.
 * @property memory   WebAssembly.Memory object.
 * @property bitset   Bitset indicating liveness of each MBlock.
 */
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

  /**
   * Allocates n MBlocks
   */
  getMBlocks(n) {
    // Try to find n unused contiguous MBlocks to reuse
    //
    // TODO: using a kind of fold may be faster: the accumulator would be the
    // number of contiguous unused blocks. If the accumulator is greater than n,
    // return current_mblock-n
    const m = mask(n);
    for (let i = BigInt(0); i <= BigInt(this.capacity - n); ++i) {
      const mi = m << i;
      if (!(this.bitset & mi)) {
        this.bitset |= mi;
        return Memory.tagData(Number(i) * rtsConstants.mblock_size);
      }
    }

    // Allocate fresh MBlocks: either by doubling the capacity (TODO: why?)
    // or by allocating n new blocks if n > current capcity
    const d = Math.max(n, this.capacity),
      prev_capacity = this.capacity;
    this.memory.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
    this.capacity += d;
    this.bitset |= m << BigInt(prev_capacity); // mark mblocks as used
    return Memory.tagData(prev_capacity * rtsConstants.mblock_size);
  }

  /**
   * Free n MBlocks starting from p
   *
   * Note: the MBlocks are still allocated in the underlying WebAssembly.Memory
   * object but are marked as unused and may be reused later.
   */
  free(p, n) {
    const mblock_no =
      // TODO: make the log2 a constant (MBLOCK_SHIFT as in GHC) and use it
      // everywhere we multiply or divide by mblock_size
      BigInt(Memory.unTag(p)) >> BigInt(Math.log2(rtsConstants.mblock_size));
    this.bitset &= ~(mask(n) << mblock_no);
  }
}
