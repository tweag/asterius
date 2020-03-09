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
     * An array containing the addresses of
     * the (block descriptors of the) MBlocks
     * allocated for each generation.
     * @name HeapAlloc#generations
     */
    this.generations = new Array(2); // 2 generations
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
    this.setGenerationNo(0);
    this.currentPools[1] = this.allocMegaGroup(1, true);
  }
  /**
   * Sets the current generation number, so that new closures and 
   * MBlocks are allocated in the right space and with correct flag.
   * @param {number} gen_no The generation number
   * @param {boolean} [forceNewAlloc=true] Force the allocation
   *   of a new MBlock. 
   */
  setGenerationNo(gen_no, forceNewAlloc=true) {
    let pool = this.generations[gen_no];
    if (forceNewAlloc || !pool) {
      pool = this.allocMegaGroup(1, false, gen_no);
      this.generations[gen_no] = pool;
    }
    this.currentPools[0] = pool;
  }

  /**
   * Allocates a new MegaGroup of enough MBlocks to
   * accommodate the supplied amount of bytes.
   * @param b The number of bytes to allocate
   * @param pinned Whether the MBlocks should be pinned
   * @param gen_no The generation number
   * @returns The address of the block descriptor
   *  of the first MBlock of the MegaGroup.
   */
  hpAlloc(b, pinned=false, gen_no=0) {
    const mblocks =
        b <= rtsConstants.sizeof_first_mblock
          ? 1
          : 1 +
            Math.ceil(
              (b - rtsConstants.sizeof_first_mblock) / rtsConstants.mblock_size
            ),
      bd = this.allocMegaGroup(mblocks, pinned, gen_no);
    return bd;
  }

  /**
   * Allocates enough blocks to accommodate the given number
   * of words in the appropriate pool.
   * @param n The number of (64 bit) words to allocate
   * @param pinned Whether to allocate in the pinned pool
   */
  allocate(n, pinned = false) {
    const b = n << 3; // The size in bytes
    // Large objects are forced to be pinned as well
    // (by large, we mean >= 4KiB):
    pinned = pinned || b >= rtsConstants.block_size;
    let pool = this.currentPools[Number(pinned)],
      current_start = Number(
        this.memory.i64Load(pool + rtsConstants.offset_bdescr_start)
      ),
      current_free = Number(
        this.memory.i64Load(pool + rtsConstants.offset_bdescr_free)
      );
    const current_blocks = this.memory.i32Load(
        pool + rtsConstants.offset_bdescr_blocks
      ),
      current_limit = current_start + rtsConstants.block_size * current_blocks,
      new_free = current_free + b;

    if (new_free <= current_limit) {
      // if the pool has enough space
      this.memory.i64Store(
        pool + rtsConstants.offset_bdescr_free,
        new_free
      );
    } else {
      // not enough space in the corresponding pool,
      // allocate a new one
      if (pinned) {
        pool = this.hpAlloc(b, true);
        this.currentPools[1] = pool;
      } else {
        const gen_no = this.memory.i16Load(pool + rtsConstants.offset_bdescr_gen_no);
        pool = this.hpAlloc(b, false, gen_no);
        this.currentPools[0] = pool;
        this.generations[gen_no] = pool;
      }
      current_free = Number(
        this.memory.i64Load(
          pool + rtsConstants.offset_bdescr_free
        )
      );
      this.memory.i64Store(
        pool + rtsConstants.offset_bdescr_free,
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
   * Allocates a new MegaGroup of size the supplied number of MBlocks.
   * @param n The number of requested MBlocks
   * @param pinned Whether the MBlocks should be pinned
   * @param gen_no The generation number
   * @return The address of the block descriptor
   *  of the first MBlock of the MegaGroup
   */
  allocMegaGroup(n, pinned=false, gen_no=0) {
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
    this.memory.i16Store(
      bd + rtsConstants.offset_bdescr_flags,
      pinned ? rtsConstants.BF_PINNED : 0
    );
    this.memory.i16Store(bd + rtsConstants.offset_bdescr_gen_no, gen_no);
    this.mgroups.add(bd);
    return bd;
  }

  /**
   * Frees the garbage MBlocks by taking into account the
   * information on live and dead MBlocks passed by the 
   * garbage collector. Used by {@link GC#performGC}.
   * @param live_mblocks The set of current live MBlocks
   * @param live_mblocks The set of current dead MBlocks
   * @param major Whether this info comes from a minor or major GC
   */
  handleLiveness(live_mblocks, dead_mblocks, major=true) {
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
        const
          gen_no = this.memory.i16Load(bd + rtsConstants.offset_bdescr_gen_no),
          pinned = Boolean(
            this.memory.i16Load(bd + rtsConstants.offset_bdescr_flags) & rtsConstants.BF_PINNED
          );
        // Note: not all unreachable MBlocks can be 
        // freed during a minor collection. This is because
        // pinned MBlocks or older MBlocks may look unreachable
        // since only the pointers to younger generations
        // are stored in the remembered set.
        if(major || (!pinned && gen_no == 0)) {
          this.mgroups.delete(bd);
          const p = bd - rtsConstants.offset_first_bdescr,
            n = this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
          this.memory.freeMBlocks(p, n);
        }
      }
    }
    // Reallocate pinned pool if the current has been freed
    if (!this.mgroups.has(this.currentPools[1])) {
      this.currentPools[1] = this.allocMegaGroup(1, true);
    }
    // Reinitialize generations if necessary
    for (let i=0; i < this.generations.length; i++)
      if (!this.mgroups.has(this.generations[i])) {
        this.generations[i] = undefined;
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
