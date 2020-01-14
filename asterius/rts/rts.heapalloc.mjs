import * as rtsConstants from "./rts.constants.mjs";

/**
 * Class implementing the allocation of nurseries,
 * and also individual heap objects.
 * In the asterius RTS - contrary to GHC - we don't
 * really distinguish between "blocks" and "MBlocks"
 * ("megablocks", "em-blocks"); here all blocks are
 * really MBlocks. MBlocks have a fixed size of 1MiB
 * and are allocated by {@link Memory}. Moreover,
 * MBlocks can be chained to form MegaGroups.
 * For more information on (mega)block allocation, see
 * {@link https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/block-alloc}.
 */
export class HeapAlloc {
  constructor(memory) {
    /**
     * @type Memory
     * @name HeapAlloc#memory
     */
    this.memory = memory;
    /**
     * An array with two entries:
     * 1. The unpinned pool, i.e. the address of the
     *    block descriptor for the MBlock where
     *    unpinned objects are allocated,
     * 2. The pinned pool, i.e. the address of the
     *    block descriptor for the MBlock where
     *    pinned objects are allocated.
     * @name HeapAlloc#currentPools
     */
    this.currentPools = [undefined, undefined];
    /**
     * The set of all currently allocated MegaGroups.
     */
    this.mgroups = new Set();
    Object.freeze(this);
  }

  /**
   * Initializes the pinned & unpinned pools.
   */
  init() {
    this.currentPools[0] = this.allocMegaGroup(1);
    this.currentPools[1] = this.allocMegaGroup(1);
    this.memory.i16Store(
      this.currentPools[1] + rtsConstants.offset_bdescr_flags,
      rtsConstants.BF_PINNED
    );
  }
  /**
   * Initializes only the unpinned pool.
   */
  initUnpinned() {
    this.currentPools[0] = this.allocMegaGroup(1);
  }

  /**
   * Allocates a new MegaGroup of enough MBlocks to
   * accommodate the supplied amount of bytes.
   * @param b The number of bytes to allocate
   * @returns The address of the block descriptor
   *  of the first MBlock of the MegaGroup.
   */
  hpAlloc(b) {
    const mblocks =
        b <= rtsConstants.sizeof_first_mblock
          ? 1
          : 1 +
            Math.ceil(
              (b - rtsConstants.sizeof_first_mblock) / rtsConstants.mblock_size
            ),
      bd = this.allocMegaGroup(mblocks);
    return bd;
  }

  /**
   * Allocates enough blocks to accommodate the given number
   * of words in the appropriate pool.
   * @param n The number of (64 bit) words to allocate
   * @param pinned Whether to allocate in the pinned pool
   */
  allocate(n, pinned = false) {
    let b = n << 3, // The size in bytes
      // Large objects are forced to be pinned as well
      // (by large, we mean >= 4KiB):
      pool_i = Number(pinned || b >= rtsConstants.block_size),
      current_start = Number(
        this.memory.i64Load(
          this.currentPools[pool_i] + rtsConstants.offset_bdescr_start
        )
      ),
      current_free = Number(
        this.memory.i64Load(
          this.currentPools[pool_i] + rtsConstants.offset_bdescr_free
        )
      ),
      current_blocks = this.memory.i32Load(
        this.currentPools[pool_i] + rtsConstants.offset_bdescr_blocks
      ),
      current_limit = current_start + rtsConstants.block_size * current_blocks,
      new_free = current_free + b;

    if (new_free <= current_limit) {
      // if the pool has enough space
      this.memory.i64Store(
        this.currentPools[pool_i] + rtsConstants.offset_bdescr_free,
        new_free
      );
    } else {
      // not enough space in the corresponding pool,
      // allocate a new one
      this.currentPools[pool_i] = this.hpAlloc(b);
      if (pool_i)
        this.memory.i16Store(
          this.currentPools[pool_i] + rtsConstants.offset_bdescr_flags,
          rtsConstants.BF_PINNED
        );
      current_free = Number(
        this.memory.i64Load(
          this.currentPools[pool_i] + rtsConstants.offset_bdescr_free
        )
      );
      this.memory.i64Store(
        this.currentPools[pool_i] + rtsConstants.offset_bdescr_free,
        current_free + b
      );
    }
    return current_free;
  }

  /**
   * Allocates the given number of words in the pinned pool.
   * @param n The number of (64 bit) words to allocate
   */
  allocatePinned(n) {
    return this.allocate(n, true);
  }

  /**
   * Allocates a new MegaGroup of size the supplies number of MBlocks.
   * @param n The number of requested MBlocks
   * @return The address of the block descriptor
   *  of the first MBlock of the MegaGroup
   */
  allocMegaGroup(n) {
    const req_blocks =
        (rtsConstants.mblock_size * n - rtsConstants.offset_first_block) /
        rtsConstants.block_size,
      mblock = this.memory.getMBlocks(n),
      bd = mblock + rtsConstants.offset_first_bdescr,
      block_addr = mblock + rtsConstants.offset_first_block;
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
    this.memory.i16Store(bd + rtsConstants.offset_bdescr_node, n);
    this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
    this.mgroups.add(bd);
    return bd;
  }

  /**
   * Frees the garbage MBlocks by taking into account the
   * information on live and dead MBlocks passed by the 
   * garbage collector. Used by {@link GC#performGC}.
   * @param live_mblocks The set of current live MBlocks
   * @param live_mblocks The set of current dead MBlocks
   */
  handleLiveness(live_mblocks, dead_mblocks) {
    for (const bd of live_mblocks) {
      if (!this.mgroups.has(bd)) {
        throw new WebAssembly.RuntimeError(
          `Invalid live mblock 0x${bd.toString(16)}`
        );
      }
    }
    // Free MBlocks that have been copied during GC
    for (const bd of dead_mblocks) {
      if (!this.mgroups.has(bd)) {
        throw new WebAssembly.RuntimeError(
          `Invalid dead mblock 0x${bd.toString(16)}`
        );
      }
      this.mgroups.delete(bd);
      const p = bd - rtsConstants.offset_first_bdescr,
        n = this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
      this.memory.freeMBlocks(p, n);
    }
    // Free unreachable MBlocks
    for (const bd of Array.from(this.mgroups)) {
      if (!live_mblocks.has(bd)) {
        this.mgroups.delete(bd);
        const p = bd - rtsConstants.offset_first_bdescr,
          n = this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
        this.memory.freeMBlocks(p, n);
      }
    }
    // Reinitialize pools if necessary
    if (!this.mgroups.has(this.currentPools[0])) {
      this.currentPools[0] = this.allocMegaGroup(1);
    }
    if (!this.mgroups.has(this.currentPools[1])) {
      this.currentPools[1] = this.allocMegaGroup(1);
      this.memory.i16Store(
        this.currentPools[1] + rtsConstants.offset_bdescr_flags,
        rtsConstants.BF_PINNED
      );
    }
  }

  /**
   * Estimates the size of living objects by counting the number
   * of MBlocks that were allocated by {@link GC#getMBlocks} 
   * some time ago, but have not been yet been freed by {@link GC#freeMBlocks}.
   * @returns The number of allocated MBlocks
   */
  liveSize() {
    let acc = 0;
    for (const bd of this.mgroups) {
      acc += this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
    }
    return acc;
  }
}
