import * as rtsConstants from "./rts.constants.mjs";
import { isI32 } from "./rts.typecheck.mjs";

/**
 * Class implementing the allocation of nurseries,
 * and also individual heap objects.
 * For more information on (mega)block allocation, see
 * {@link https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/block-alloc}.
 */
export class HeapAlloc {
  constructor(components) {
    this.components = components;
    /**
     * An array with two entries:
     * 1. The unpinned pool, i.e. the address of the
     *    block descriptor for the block where
     *    unpinned objects are allocated,
     * 2. The pinned pool, i.e. the address of the
     *    block descriptor for the block where
     *    pinned objects are allocated.
     * @name HeapAlloc#currentPools
     */
    this.currentPools = [undefined, undefined];
    /**
     * An array containing the addresses of
     * the (block descriptors of the) block
     * allocated for each generation.
     * @name HeapAlloc#generations
     */
    this.generations = new Array(2); // 2 generations
    /**
     * The set of all currently allocated MegaGroups.
     */
    this.bgroups = new Set();
    Object.freeze(this);
  }

  /**
   * Initializes the pinned & unpinned pools.
   */
  init() {
    this.setGenerationNo(0);
    this.currentPools[1] = this.allocBlockGroup(1, true);
  }
  /**
   * Sets the current generation number, so that new closures and
   * blocks are allocated in the right space and with correct flag.
   * @param {number} gen_no The generation number
   * @param {boolean} [forceNewAlloc=true] Force the allocation
   *   of a new block.
   */
  setGenerationNo(gen_no, forceNewAlloc=true) {
    let pool = this.generations[gen_no];
    if (forceNewAlloc || !pool) {
      pool = this.allocBlockGroup(1, false, gen_no);
      this.generations[gen_no] = pool;
    }
    this.currentPools[0] = pool;
  }

  /**
   * Allocates a new block group of enough blocks to
   * accommodate the supplied amount of bytes.
   * @param b The number of bytes to allocate
   * @param pinned Whether the blocks should be pinned
   * @param gen_no The generation number
   * @returns The address of the block descriptor
   *  of the first block of the block group.
   */
  hpAlloc(b, pinned=false, gen_no=0) {
    isI32(b);
    const blocks = Math.ceil(b / rtsConstants.block_size),
      bd = this.allocBlockGroup(blocks, pinned, gen_no);
    return isI32(bd);
  }

  /**
   * Allocates enough blocks to accommodate the given number
   * of words in the appropriate pool.
   * @param n The number of (64 bit) words to allocate
   * @param pinned Whether to allocate in the pinned pool
   */
  allocate(n, pinned = false) {
    isI32(n);
    const b = n << 2; // The size in bytes
    // Large objects are forced to be pinned as well
    // (by large, we mean >= 4KiB):
    pinned = pinned || b >= rtsConstants.block_size;
    let pool = this.currentPools[Number(pinned)],
      current_start = (
        this.components.memory.i32Load(pool + rtsConstants.offset_bdescr_start)
      ),
      current_free = (
        this.components.memory.i32Load(pool + rtsConstants.offset_bdescr_free)
      );
    const current_blocks = this.components.memory.i32Load(
        pool + rtsConstants.offset_bdescr_blocks
      ),
      current_limit = current_start + rtsConstants.block_size * current_blocks,
      new_free = current_free + b;

    if (new_free <= current_limit) {
      // if the pool has enough space
      this.components.memory.i32Store(
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
        const gen_no = this.components.memory.i16Load(pool + rtsConstants.offset_bdescr_gen_no);
        pool = this.hpAlloc(b, false, gen_no);
        this.currentPools[0] = pool;
        this.generations[gen_no] = pool;
      }
      current_free = (
        this.components.memory.i32Load(
          pool + rtsConstants.offset_bdescr_free
        )
      );
      this.components.memory.i32Store(
        pool + rtsConstants.offset_bdescr_free,
        current_free + b
      );
    }
    return isI32(current_free);
  }

  /**
   * Allocates the given number of words in the pinned pool.
   * @param n The number of (64 bit) words to allocate
   */
  allocatePinned(n) {
    return this.allocate(n, true);
  }

  /**
   * Allocates a new block group of size the supplied number of blocks.
   * @param n The number of requested blocks
   * @param pinned Whether the blocks should be pinned
   * @param gen_no The generation number
   * @return The address of the block descriptor
   *  of the first block of the block group
   */
  allocBlockGroup(n, pinned=false, gen_no=0) {
    const bd = this.components.exports.allocGroup(n);
    this.components.memory.i16Store(
      bd + rtsConstants.offset_bdescr_flags,
      pinned ? rtsConstants.BF_PINNED : 0
    );
    this.components.memory.i16Store(bd + rtsConstants.offset_bdescr_gen_no, gen_no);
    this.bgroups.add(bd);
    return bd;
  }

  /**
   * Frees the garbage blocks by taking into account the
   * information on live and dead blocks passed by the
   * garbage collector. Used by {@link GC#performGC}.
   * @param live_blocks The set of current live blocks
   * @param live_blocks The set of current dead blocks
   * @param major Whether this info comes from a minor or major GC
   */
  handleLiveness(live_blocks, dead_blocks, major=true) {
    for (const bd of live_blocks) {
      if (!this.bgroups.has(bd)) {
        throw new WebAssembly.RuntimeError(
          `Invalid live block 0x${bd.toString(16)}`
        );
      }
    }
    // Free blocks that have been copied during GC
    for (const bd of dead_blocks) {
      if (!this.bgroups.has(bd)) {
        throw new WebAssembly.RuntimeError(
          `Invalid dead block 0x${bd.toString(16)}`
        );
      }
      this.bgroups.delete(bd);
      this.components.exports.freeGroup(bd);
    }

    // Free unreachable blocks
    for (const bd of Array.from(this.bgroups)) {
      if (!live_blocks.has(bd)) {
        const
          gen_no = this.components.memory.i16Load(bd + rtsConstants.offset_bdescr_gen_no),
          pinned = Boolean(
            this.components.memory.i16Load(bd + rtsConstants.offset_bdescr_flags) & rtsConstants.BF_PINNED
          );
        // Note: not all unreachable blocks can be
        // freed during a minor collection. This is because
        // pinned blocks or older blocks may look unreachable
        // since only the pointers to younger generations
        // are stored in the remembered set.
        if(major || (!pinned && gen_no == 0)) {
          this.bgroups.delete(bd);
          this.components.exports.freeGroup(bd);
        }
      }
    }
    // Reallocate pinned pool if the current has been freed
    if (!this.bgroups.has(this.currentPools[1])) {
      this.currentPools[1] = this.allocBlockGroup(1, true);
    }
    // Reinitialize generations if necessary
    for (let i=0; i < this.generations.length; i++)
      if (!this.bgroups.has(this.generations[i])) {
        this.generations[i] = undefined;
      }
  }

  /**
   * Estimates the size of living objects by counting the number
   * of blocks
   * some time ago, but have not been yet been freed.
   * @returns The size of allocated blocks
   */
  liveSize() {
    let acc = 0;
    for (const bd of this.bgroups) {
      const blocks = this.components.memory.i32Load(
        bd + rtsConstants.offset_bdescr_blocks
      );
      acc += rtsConstants.block_size * blocks;
    }
    return acc;
  }
}
