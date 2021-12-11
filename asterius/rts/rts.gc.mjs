import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as FunTypes from "./rts.funtypes.mjs";
import { Memory } from "./rts.memory.mjs";
import * as rtsConstants from "./rts.constants.mjs";
import { stg_arg_bitmaps } from "./rts.autoapply.mjs";
import { JSValManager } from "./rts.jsval.mjs";

/**
 * Class implementing copying garbage collection.
 */
export class GC {
  constructor(
    components,
    memory,
    heapalloc,
    stableptr_manager,
    stablename_manager,
    scheduler,
    symbol_table,
    reentrancy_guard,
    yolo,
    gcThreshold
  ) {
    this.components = components;
    this.memory = memory;
    this.heapAlloc = heapalloc;
    this.stablePtrManager = stableptr_manager;
    this.stableNameManager = stablename_manager;
    this.scheduler = scheduler;
    this.symbolTable = symbol_table;
    this.reentrancyGuard = reentrancy_guard;
    /**
     * 'Yolo' mode disables garbage collection altogether
     * (see {@link GC#performGC})
     * @name GC#yolo
     */
    this.yolo = yolo;
    /**
     * Garbage collection will not be performed when the
     * current size of "live" blocks is less than
     * {@link GC#gcThreshold} (see {@link GC#performGC}).
     * @name GC#gcThreshold
     * @default 64
     */
    this.gcThreshold = gcThreshold;
    /**
     * Set of closures encountered during garbage
     * collection but not moved: they are either
     * closures in the statis part of memory, or
     * closures in pinned blocks.
     * @name GC#nonMovedObjects
     */
    this.nonMovedObjects = new Set();
    /**
     * List containing the non-moved closures that
     * have not been scavenged yet.
     * @name GC#nonMovedObjectsToScavenge
     */
    this.nonMovedObjectsToScavenge = [];
    /**
     * Set containing the blocks in the to-space,
     * i.e. the blocks where reachable objects are copied
     * during garbage collection.
     * Notes:
     * 1) Pinned blocks are not copied during GC: they are
     *    simply set as live, and added to the liveBlocks set.
     * 2) Static objects are not copied either, but their
     *    blocks are not even added to the liveBlocks set.
     * @name GC#liveBlocks
     */
    this.liveBlocks = new Set();
    /**
     * List containing the blocks in the to-space
     * that have yet to be scavenged.
     * @name GC#blocksToScavenge
     */
    this.blocksToScavenge = [];
    /**
     * Set containing the blocks in the from-space,
     * i.e. the blocks containing objects that have been
     * copied into to-space. These blocks will be freed
     * at the end of garbage collection.
     * @name GC#deadBlocks
     */
    this.deadBlocks = new Set();
    /**
     * At each garbage collection, the live JSVals encountered are
     * recorded in {@link GC#liveJSValManager}.
     * @name GC#liveJSValManager
     */
    this.liveJSValManager = new JSValManager(components);
    Object.seal(this);
  }

  /**
   * Checks whether the provided memory address resides
   * in a pinned block. Used by {@link GC#evacuateClosure}
   * to avoid evacuating pinned objects.
   * @param addr The memory address to check
   */
  isPinned(addr) {
    const bd = this.components.exports.__ahc_Bdescr(addr),
      flags = this.memory.i16Load(bd + rtsConstants.offset_bdescr_flags);
    return Boolean(flags & rtsConstants.BF_PINNED);
  }

  /**
   * Heap allocates a physical copy of the given closure.
   * Used during evacuation by {@link GC#evacuateClosure}.
   * @param c The source address of the closure
   * @param bytes The size in bytes of the closure
   */
  copyClosure(c, bytes) {
    const dest_c = this.heapAlloc.allocate(0xdeadbeef, Math.ceil(bytes / 4));
    this.memory.memcpy(dest_c, c, bytes);
    const dest_block = this.components.exports.__ahc_Bdescr(dest_c);
    if (!this.liveBlocks.has(dest_block)) {
      this.blocksToScavenge.push(dest_block);
      this.liveBlocks.add(dest_block);
    }
    this.deadBlocks.add(this.components.exports.__ahc_Bdescr(c));
    return dest_c;
  }

  /**
   * Performs _stingy_ evaluation, i.e. a very frugual form
   * of evaluation that is carried during garbage collection.
   * It implements the following two optimizations:
   * - Indirections short-cutting;
   * - Selector optimization: remove thunks of applications of field
   *   selectors.
   * Only the argument `c` is required: the other arguments will be
   * computed in case they are `undefined`.
   * @param {number} c - The address of the closure
   * @param {number=} untagged_c - The unDynTag-ed address
   * @param {number=} info - The info pointer of `c`
   * @param {number=} type - The closure type of `c`
   * @returns A tuple array `[res_c, res_type]` containing
   *   the resulting address and type of the closure after
   *   the optimisation.
   */
  stingyEval(c, untagged_c, info, type) {
    if (!untagged_c) {
      // If no information about c is present, compute it
      untagged_c = Memory.unDynTag(c);
      info = (this.memory.i32Load(untagged_c));
      if (info % 2 == 0) {
        // Obtain the closure type only if the header
        // is an info pointer and not a forwarding pointer
        type = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_type
        );
      }
    }
    switch (type) {
      case ClosureTypes.IND: {
        // Whitehole
        this.memory.i32Store(
          untagged_c,
          this.symbolTable.addressOf("stg_WHITEHOLE_info")
        );
        // Follow the indirectee
        const [res_c, _] = this.stingyEval(
          (
            this.memory.i32Load(
              untagged_c + rtsConstants.offset_StgInd_indirectee
            )
          ));
        this.memory.i32Store(untagged_c, this.symbolTable.addressOf("stg_IND_info")); // Undo whiteholing
        this.memory.i32Store(untagged_c + rtsConstants.offset_StgInd_indirectee, res_c);
        return [res_c, ClosureTypes.IND];
      }
      case ClosureTypes.THUNK_SELECTOR: {
        // Whitehole
        this.memory.i32Store(
          untagged_c,
          this.symbolTable.addressOf("stg_WHITEHOLE_info")
        );
        // Follow the selectee
        const [res_c, res_type] = this.stingyEval(
          (
            this.memory.i32Load(
              untagged_c + rtsConstants.offset_StgSelector_selectee
            ))
        );
        // try to perform selection
        switch (res_type) {
          case ClosureTypes.CONSTR:
          case ClosureTypes.CONSTR_2_0:
          case ClosureTypes.CONSTR_NOCAF: {
            const offset = this.memory.i32Load(
              info + rtsConstants.offset_StgInfoTable_layout
            );
            // Warning: at this point (and in the similar point below)
            // we may be losing the dynamic tagging, fixme
            const selectee = this.memory.i32Load(
              Memory.unDynTag(res_c) + ((1 + offset) << 2)
            );
            this.memory.i32Store(untagged_c + rtsConstants.offset_StgInd_indirectee, selectee);
            // Set the current closure as IND, but do not
            // un-whitehole for now: it will be taken care
            // of later, when propagating the result
            // (see case IND above)
            return this.stingyEval(c, untagged_c, info, ClosureTypes.IND);
          }
          case ClosureTypes.CONSTR_1_0:
          case ClosureTypes.CONSTR_1_1: {
            const selectee = this.memory.i32Load(Memory.unDynTag(res_c) + 4);
            this.memory.i32Store(
              untagged_c + rtsConstants.offset_StgInd_indirectee,
              selectee
            );
            return this.stingyEval(c, untagged_c, info, ClosureTypes.IND);
          }
          default: {
            this.memory.i32Store(untagged_c, info); // Undo whiteholing
            this.memory.i32Store(
              untagged_c + rtsConstants.offset_StgSelector_selectee,
              res_c
            );
            return [c, type];
          }
        }
      }
      default: {
        return [c, type];
      }
    }
  }

  /**
   * Evacuates a closure. This consists of:
   * (1) Copying the closure into to-space through {@link GC#copyClosure}
   * (2) Map the old unDynTag-ed address of the closure
   *     to its new unDynTag-ed address in {@link GC#closureIndirects}.
   * If that closure had already been evacuated, simply
   * return the forwarding pointer already present in {@link GC#closureIndirects}.
   * @param c The memory address of the closure to evacuate.
   */
  evacuateClosure(c) {
    const tag = Memory.getDynTag(c),
      untagged_c = Memory.unDynTag(c);
    const info = (this.memory.i32Load(untagged_c));

    if (info % 2) {
      // The info header has already been overwritten with
      // a forwarding address: just follow it
      return Memory.setDynTag(info, tag);
    } else if (this.nonMovedObjects.has(untagged_c)) {
      // The closure is either pinned or static, and has
      // already been enqueued for scavenging: just return it
      return c;
    } else if (!this.components.exports.__ahc_HEAP_ALLOCED(untagged_c)) {
      // Object in the static part of the memory:
      // it won't be copied ...
      this.nonMovedObjects.add(untagged_c);
      // ... but it will still be scavenged
      this.nonMovedObjectsToScavenge.push(untagged_c);
      // Warning: do not set the block as live,
      // because the static part of memory is not
      // tracked by HeapAlloc.bgroups and it would
      // break the checks in HeapAlloc.handleLiveness.
      return c;
    } else if (this.isPinned(untagged_c)) {
      // The object belongs to a pinned block:
      // it won't be copied ...
      this.nonMovedObjects.add(untagged_c);
      // ... but it will still be scavenged
      this.nonMovedObjectsToScavenge.push(untagged_c);
      // Set the pinned block as live
      this.liveBlocks.add(this.components.exports.__ahc_Bdescr(untagged_c));
      return c;
    }
    // The closure is heap-allocated and dynamic:
    // proceed to evacuate it into to-space
    let dest_c = undefined;
    // Get the type of the closure from info tables
    let type = this.memory.i32Load(
      info + rtsConstants.offset_StgInfoTable_type
    );
    if (type == ClosureTypes.THUNK_SELECTOR || type == ClosureTypes.IND) {
      // Optimize selectors and indirections
      type = this.stingyEval((c), untagged_c, info, type)[1];
    }
    switch (type) {
      case ClosureTypes.CONSTR_0_1:
      case ClosureTypes.FUN_0_1:
      case ClosureTypes.FUN_1_0:
      case ClosureTypes.CONSTR_1_0: {
        dest_c = this.copyClosure(untagged_c, 8);
        break;
      }
      case ClosureTypes.THUNK_1_0:
      case ClosureTypes.THUNK_0_1: {
        dest_c = this.copyClosure(untagged_c, rtsConstants.sizeof_StgThunk + 4);
        break;
      }
      case ClosureTypes.THUNK_1_1:
      case ClosureTypes.THUNK_2_0:
      case ClosureTypes.THUNK_0_2: {
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgThunk + 8
        );
        break;
      }
      case ClosureTypes.FUN_1_1:
      case ClosureTypes.FUN_2_0:
      case ClosureTypes.FUN_0_2:
      case ClosureTypes.CONSTR_1_1:
      case ClosureTypes.CONSTR_2_0:
      case ClosureTypes.CONSTR_0_2: {
        dest_c = this.copyClosure(untagged_c, 24);
        break;
      }
      case ClosureTypes.THUNK: {
        const ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout
          ),
          non_ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout + 4
          );
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgThunk + ((ptrs + non_ptrs) << 2)
        );
        break;
      }
      case ClosureTypes.FUN:
      case ClosureTypes.CONSTR:
      case ClosureTypes.CONSTR_NOCAF:
      case ClosureTypes.MVAR_CLEAN:
      case ClosureTypes.MVAR_DIRTY:
      case ClosureTypes.MUT_VAR_CLEAN:
      case ClosureTypes.MUT_VAR_DIRTY:
      case ClosureTypes.WEAK:
      case ClosureTypes.PRIM:
      case ClosureTypes.MUT_PRIM:
      case ClosureTypes.BLACKHOLE: {
        const ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout
          ),
          non_ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout + 4
          );
        dest_c = this.copyClosure(untagged_c, (1 + ptrs + non_ptrs) << 2);

        if (info === this.symbolTable.addressOf("stg_JSVAL_info")) {
          this.liveJSValManager.closure2Val.set(
            dest_c,
            this.components.jsvalManager.getJSValzh(untagged_c)
          );
        }

        break;
      }
      case ClosureTypes.THUNK_SELECTOR: {
        dest_c = this.copyClosure(untagged_c, rtsConstants.sizeof_StgSelector);
        break;
      }
      case ClosureTypes.IND: {
        dest_c = this.evacuateClosure(
          this.memory.i32Load(
            untagged_c + rtsConstants.offset_StgInd_indirectee
          )
        );
        // cannot simply break here, because in the case of IND closures
        // dest_c must not be tagged with the current tag
        this.memory.i32Store(untagged_c, Memory.setDynTag(dest_c, 1));
        return dest_c;
      }
      case ClosureTypes.PAP: {
        const n_args = this.memory.i32Load(
          untagged_c + rtsConstants.offset_StgPAP_n_args
        );
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgPAP + (n_args << 2)
        );
        break;
      }
      case ClosureTypes.AP: {
        const n_args = this.memory.i32Load(
          untagged_c + rtsConstants.offset_StgAP_n_args
        );
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgAP + (n_args << 2)
        );
        break;
      }
      case ClosureTypes.AP_STACK: {
        const size = (
          this.memory.i32Load(untagged_c + rtsConstants.offset_StgAP_STACK_size)
        );
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgAP_STACK + (size << 2)
        );
        break;
      }
      case ClosureTypes.ARR_WORDS: {
        dest_c = this.copyClosure(
          untagged_c,
          Math.ceil(
            (rtsConstants.sizeof_StgArrBytes +
              (
                this.memory.i32Load(
                  untagged_c + rtsConstants.offset_StgArrBytes_bytes
                )
              )) /
              4
          ) * 4
        );
        break;
      }
      case ClosureTypes.MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_CLEAN: {
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgMutArrPtrs +
            ((
              this.memory.i32Load(
                untagged_c + rtsConstants.offset_StgMutArrPtrs_ptrs
              )
            ) <<
              2)
        );
        break;
      }
      case ClosureTypes.SMALL_MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: {
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgSmallMutArrPtrs +
            ((
              this.memory.i32Load(
                untagged_c + rtsConstants.offset_StgSmallMutArrPtrs_ptrs
              )
            ) <<
              2)
        );
        break;
      }
      default:
        throw new WebAssembly.RuntimeError();
    }
    // Overwrite the object header with a forwarding
    // pointer (i.e. store the address with the
    // least significant bit set to 1)
    this.memory.i32Store(untagged_c, dest_c + 1);
    // Finally, return the new address
    return Memory.setDynTag(dest_c, tag);
  }

  scavengeClosureAt(p) {
    this.memory.i32Store(p, this.evacuateClosure(this.memory.i32Load(p)));
  }

  scavengePointersFirst(payload, ptrs) {
    for (let i = 0; i < ptrs; ++i) this.scavengeClosureAt(payload + (i << 2));
  }

  scavengeSmallBitmap(payload, bitmap, size) {
    for (let i = 0; i < size; ++i)
      if (!((bitmap >> (i)) & 1))
        this.scavengeClosureAt(payload + (i << 2));
  }

  scavengeLargeBitmap(payload, large_bitmap, size) {
    for (let j = 0; j < size; j += 32) {
      const bitmap = this.memory.i32Load(
        large_bitmap + rtsConstants.offset_StgLargeBitmap_bitmap + (j >> 2)
      );
      for (let i = j; i - j < 32 && i < size; ++i)
        if (!((bitmap >> (i - j)) & 1))
          this.scavengeClosureAt(payload + (i << 2));
    }
  }

  scavengePAP(c, offset_fun, payload, n_args) {
    this.scavengeClosureAt(c + offset_fun);
    const fun = this.memory.i32Load(c + offset_fun),
      fun_info = (this.memory.i32Load(Memory.unDynTag(fun)));
    switch (
      this.memory.i32Load(
        fun_info +
          rtsConstants.offset_StgFunInfoTable_f +
          rtsConstants.offset_StgFunInfoExtraFwd_fun_type
      )
    ) {
      case FunTypes.ARG_GEN: {
        this.scavengeSmallBitmap(
          payload,
          this.memory.i32Load(
            fun_info +
              rtsConstants.offset_StgFunInfoTable_f +
              rtsConstants.offset_StgFunInfoExtraFwd_b
          ) >> (5),
          n_args
        );
        break;
      }
      case FunTypes.ARG_GEN_BIG: {
        this.scavengeLargeBitmap(
          payload,
          (
            this.memory.i32Load(
              fun_info +
                rtsConstants.offset_StgFunInfoTable_f +
                rtsConstants.offset_StgFunInfoExtraFwd_b
            )
          ),
          n_args
        );
        break;
      }
      case FunTypes.ARG_BCO: {
        throw new WebAssembly.RuntimeError();
      }
      default: {
        this.scavengeSmallBitmap(
          payload,
          (
            stg_arg_bitmaps[
              this.memory.i32Load(
                fun_info +
                  rtsConstants.offset_StgFunInfoTable_f +
                  rtsConstants.offset_StgFunInfoExtraFwd_fun_type
              )
            ]
          ) >> (5),
          n_args
        );
        break;
      }
    }
  }

  scavengeStackChunk(sp, sp_lim) {
    let c = sp;
    while (true) {
      if (c > sp_lim) throw new WebAssembly.RuntimeError();
      if (c == sp_lim) break;
      const info = (this.memory.i32Load(c)),
        type = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_type
        ),
        raw_layout = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout
        );
      if (this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_srt))
        this.evacuateClosure(
          this.memory.i32Load(info + rtsConstants.offset_StgRetInfoTable_srt)
        );
      switch (type) {
        case ClosureTypes.RET_SMALL:
        case ClosureTypes.UPDATE_FRAME:
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.UNDERFLOW_FRAME:
        case ClosureTypes.STOP_FRAME:
        case ClosureTypes.ATOMICALLY_FRAME:
        case ClosureTypes.CATCH_RETRY_FRAME:
        case ClosureTypes.CATCH_STM_FRAME: {
          const size = raw_layout & 0x1f,
            bitmap = raw_layout >> (5);
          this.scavengeSmallBitmap(c + 4, bitmap, size);
          c += (1 + size) << 2;
          break;
        }
        case ClosureTypes.RET_BIG: {
          const size = (
            this.memory.i32Load(
              raw_layout + rtsConstants.offset_StgLargeBitmap_size
            )
          );
          this.scavengeLargeBitmap(c + 4, raw_layout, size);
          c += (1 + size) << 2;
          break;
        }

        // https://github.com/ghc/ghc/blob/2ff77b9894eecf51fa619ed2266ca196e296cd1e/rts/Printer.c#L609
        // https://github.com/ghc/ghc/blob/2ff77b9894eecf51fa619ed2266ca196e296cd1e/rts/sm/Scav.c#L1944
        case ClosureTypes.RET_FUN: {
          const retfun = c;
          const size = (
            this.memory.i32Load(retfun + rtsConstants.offset_StgRetFun_size)
          );

          // NOTE: the order is important. The scavenging will move all the
          // data inside, so that when we grab "fun", we grab the right fun
          // that has been moved.
          this.scavengeClosureAt(retfun + rtsConstants.offset_StgRetFun_fun);
          let fun = (
            this.memory.i32Load(retfun + rtsConstants.offset_StgRetFun_fun)
          );
          const fun_info_p = fun + 0;
          const fun_info = (
            this.memory.i32Load(Memory.unDynTag(fun_info_p))
          );

          const fun_type = this.memory.i32Load(
            fun_info +
              rtsConstants.offset_StgFunInfoTable_f +
              rtsConstants.offset_StgFunInfoExtraFwd_fun_type
          );

          const ret_fun_payload =
            retfun + rtsConstants.offset_StgRetFun_payload;

          switch (fun_type) {
            case FunTypes.ARG_GEN: {
              this.scavengeSmallBitmap(
                c + rtsConstants.offset_StgRetFun_payload,
                this.memory.i32Load(
                  fun_info +
                    rtsConstants.offset_StgFunInfoTable_f +
                    rtsConstants.offset_StgFunInfoExtraFwd_b
                ) >> 5,
                size
              );
              break;
            }
            case FunTypes.ARG_GEN_BIG: {
              this.scavengeLargeBitmap(
                c + rtsConstants.offset_StgRetFun_payload,
                (
                  this.memory.i32Load(
                    fun_info +
                      rtsConstants.offset_StgFunInfoTable_f +
                      rtsConstants.offset_StgFunInfoExtraFwd_b
                  )
                ),
                size
              );
              break;
            }
            case FunTypes.ARG_BCO: {
              throw new WebAssembly.RuntimeError();
            }
            default: {
              // https://github.com/ghc/ghc/blob/bf73419518ca550e85188616f860961c7e2a336b/includes/rts/Constants.h#L186
              const BITMAP_SIZE_MASK = 0x1f;
              const BITMAP_BITS_SHIFT = 5;
              const bitmap = stg_arg_bitmaps[fun_type];

              // https://github.com/ghc/ghc/blob/2ff77b9894eecf51fa619ed2266ca196e296cd1e/includes/rts/storage/InfoTables.h#L116
              const bitmap_bits = (bitmap) >> (BITMAP_BITS_SHIFT);
              const bitmap_size = bitmap & BITMAP_SIZE_MASK;

              this.scavengeSmallBitmap(
                ret_fun_payload,
                bitmap_bits,
                bitmap_size
              );

              break;
            } // end case default
          } //end switch (fun_type)
          c += rtsConstants.sizeof_StgRetFun + (size << 2);
          break;
        }
        default:
          throw new WebAssembly.RuntimeError();
      }
    }
  }

  /**
   * Loops over all reachable objects and scavenges them.
   */
  scavengeLoop() {
    const closures = this.nonMovedObjectsToScavenge,
          blocks = this.blocksToScavenge;

    let currentBlock = undefined, currentObject = undefined;

    // Note: there are various nested loops, mainly because there are
    // two kinds of objects, that must be scavenged in a different way:
    // objects that have been copied in to-space, and non-moved objects.
    // Objects copied in to-space are scavenged by traversing the
    // to-space sequentially. Non-moved objects are stored
    // in `this.nonMovedObjects` and must be handled separately.
    // Moreover, scavenging an object of either kind may introduce
    // new objects of either kind.
    while (true) {
      if (!currentBlock) {
        // We try and pick a new block to scavenge
        currentBlock = blocks.pop();
        if (currentBlock)
          // If there exists a block to scavenge,
          // start with the object pointed
          // by the `start` field in the block
          // descriptor
          currentObject = (
            this.memory.i32Load(
              currentBlock + rtsConstants.offset_bdescr_start
            )
          );
      }
      // Iterate over the objects in the `currentBlock`,
      // but only if there's such a block
      while (currentBlock) {
        // `currentLimit` is the upper limit for `currentBlock`
        // and consists of a pointer to the free space in the
        // current block
        const currentLimit = (
          this.memory.i32Load(
            currentBlock + rtsConstants.offset_bdescr_free
          )
        );
        if (currentObject >= currentLimit)
          // There are no more blocks to scavenge in the
          // `currentBlock`. Break, but do not unset
          // the current block, as we are not done with
          // it yet: scavenging the non-moved closures below
          // may add new objects to `currentBlock`.
          break;
        // Scavenge the current object, and increase the
        // `currentObject` address of the amount (sizeof) provided by
        // the `scavengeClosure` function.
        currentObject += this.scavengeClosure(currentObject);
      }
      if (blocks.length > 0) {
        // There are more blocks to scavenge:
        // since we have completely processed the
        // current currentBlock, we can continue
        // and pick the next one
        currentBlock = currentObject = undefined;
        continue;
      } else if (closures.length == 0)
        // There are no more block to scavenge,
        // nor in the to-space nor among the non-moved
        // objects. We are done.
        return;
      // Scavenge the remaining non-moved objects
      while (closures.length > 0) {
        this.scavengeClosure(closures.pop());
      }
      // Continue scavenging the possibly newly evacuated objects
    }
  }

  /**
   * Scavenges a single object in to-space by evacuating
   * each pointer in the object, and replacing the pointer
   * with the address obtained after evacuation.
   * @param c The address of the closure to scavenge
   * @returns The size (in bytes) of the closure c
   */
  scavengeClosure(c) {
    const info = (this.memory.i32Load(c)),
      type = this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_type);
    switch (type) {
      case ClosureTypes.CONSTR_1_0: {
        this.scavengePointersFirst(c + 4, 1);
        return 8;
      }
      case ClosureTypes.CONSTR_0_1: {
        return 8;
      }
      case ClosureTypes.CONSTR_1_1: {
        this.scavengePointersFirst(c + 4, 1);
        return 12;
      }
      case ClosureTypes.CONSTR_2_0: {
        this.scavengePointersFirst(c + 4, 2);
        return 12;
      }
      case ClosureTypes.CONSTR_0_2: {
        return 12;
      }
      case ClosureTypes.FUN:
      case ClosureTypes.FUN_1_0:
      case ClosureTypes.FUN_0_1:
      case ClosureTypes.FUN_2_0:
      case ClosureTypes.FUN_1_1:
      case ClosureTypes.FUN_0_2:
      case ClosureTypes.FUN_STATIC: {
        if (this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_srt))
          this.evacuateClosure(
            this.memory.i32Load(
              info +
                rtsConstants.offset_StgFunInfoTable_f +
                rtsConstants.offset_StgFunInfoExtraFwd_srt
            )
          );
        const ptrs = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout
        ),
        non_ptrs = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout + 4
        );
        this.scavengePointersFirst(c + 4, ptrs);
        return (1 + ptrs + non_ptrs) << 2;
      }
      case ClosureTypes.CONSTR:
      case ClosureTypes.CONSTR_NOCAF:
      case ClosureTypes.BLACKHOLE:
      case ClosureTypes.MUT_VAR_CLEAN:
      case ClosureTypes.MUT_VAR_DIRTY:
      case ClosureTypes.PRIM:
      case ClosureTypes.MUT_PRIM:
      case ClosureTypes.COMPACT_NFDATA: {
        const ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout
          ),
          non_ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout + 4
          );
        this.scavengePointersFirst(c + 4, ptrs);
        return (1 + ptrs + non_ptrs) << 2;
      }
      case ClosureTypes.THUNK_STATIC:
      case ClosureTypes.THUNK:
      case ClosureTypes.THUNK_1_0:
      case ClosureTypes.THUNK_0_1:
      case ClosureTypes.THUNK_2_0:
      case ClosureTypes.THUNK_1_1:
      case ClosureTypes.THUNK_0_2: {
        if (this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_srt))
          this.evacuateClosure(
            this.memory.i32Load(
              info + rtsConstants.offset_StgThunkInfoTable_srt
            )
          );
        const ptrs = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout
        ),
        non_ptrs = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout + 4
        );
        this.scavengePointersFirst(
          c + rtsConstants.offset_StgThunk_payload,
          ptrs
        );
        return rtsConstants.sizeof_StgThunk + ((ptrs + non_ptrs) << 2);
      }
      case ClosureTypes.THUNK_SELECTOR: {
        if (this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_srt))
          this.evacuateClosure(
            this.memory.i32Load(
              info + rtsConstants.offset_StgThunkInfoTable_srt
            )
          );
        this.scavengeClosureAt(c + rtsConstants.offset_StgSelector_selectee);
        return rtsConstants.sizeof_StgSelector;
      }
      case ClosureTypes.AP: {
        const n_args = this.memory.i32Load(
          c + rtsConstants.offset_StgAP_n_args
        );
        this.scavengePAP(
          c,
          rtsConstants.offset_StgAP_fun,
          c + rtsConstants.offset_StgAP_payload,
          n_args
        );
        return rtsConstants.sizeof_StgAP + (n_args << 2);
      }
      case ClosureTypes.PAP: {
        const n_args = this.memory.i32Load(
          c + rtsConstants.offset_StgPAP_n_args
        );
        this.scavengePAP(
          c,
          rtsConstants.offset_StgPAP_fun,
          c + rtsConstants.offset_StgPAP_payload,
          n_args
        );
        return rtsConstants.sizeof_StgPAP + (n_args << 2);
      }
      case ClosureTypes.AP_STACK: {
        const size = (
          this.memory.i32Load(
            c + rtsConstants.offset_StgAP_STACK_size
          )
        );
        this.scavengeClosureAt(c + rtsConstants.offset_StgAP_STACK_fun);
        this.scavengeStackChunk(
          c + rtsConstants.offset_StgAP_STACK_payload,
          c +
            rtsConstants.offset_StgAP_STACK_payload + size
        );
        return rtsConstants.sizeof_StgAP_STACK + (size << 2);
      }
      case ClosureTypes.IND_STATIC: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgIndStatic_indirectee);
        return; // size not important, this object won't be moved
      }
      case ClosureTypes.MVAR_CLEAN:
      case ClosureTypes.MVAR_DIRTY: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgMVar_head);
        this.scavengeClosureAt(c + rtsConstants.offset_StgMVar_tail);
        this.scavengeClosureAt(c + rtsConstants.offset_StgMVar_value);
        return rtsConstants.offset_StgMVar_value + 4;
      }
      case ClosureTypes.ARR_WORDS: {
        return (
          Math.ceil(
            (rtsConstants.sizeof_StgArrBytes +
              (
                this.memory.i32Load(c + rtsConstants.offset_StgArrBytes_bytes)
              )) /
              4
          ) * 4
        );
      }
      case ClosureTypes.MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_CLEAN: {
        const ptrs = (
          this.memory.i32Load(c + rtsConstants.offset_StgMutArrPtrs_ptrs)
        );
        this.scavengePointersFirst(
          c + rtsConstants.offset_StgMutArrPtrs_payload,
          ptrs
        );
        return rtsConstants.sizeof_StgMutArrPtrs + (ptrs << 2);
      }
      case ClosureTypes.WEAK: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgWeak_cfinalizers);
        this.scavengeClosureAt(c + rtsConstants.offset_StgWeak_key);
        this.scavengeClosureAt(c + rtsConstants.offset_StgWeak_value);
        this.scavengeClosureAt(c + rtsConstants.offset_StgWeak_finalizer);
        return rtsConstants.offset_StgWeak_link + 4;
      }
      case ClosureTypes.TSO: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgTSO_stackobj);
        return; // size not important, this object won't be moved
      }
      case ClosureTypes.STACK: {
        const
          stack_size =
            this.memory.i32Load(c + rtsConstants.offset_StgStack_stack_size) << 2,
          sp = (this.memory.i32Load(c + rtsConstants.offset_StgStack_sp)),
          sp_lim = c + rtsConstants.offset_StgStack_stack + stack_size;
        this.scavengeStackChunk(sp, sp_lim);
        return rtsConstants.offset_StgStack_stack + stack_size;
      }
      case ClosureTypes.SMALL_MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: {
        const ptrs = (
          this.memory.i32Load(c + rtsConstants.offset_StgSmallMutArrPtrs_ptrs)
        );
        this.scavengePointersFirst(
          c + rtsConstants.offset_StgSmallMutArrPtrs_payload,
          ptrs
        );
        return rtsConstants.offset_StgSmallMutArrPtrs_payload + (ptrs << 2);
      }
      default:
        throw new WebAssembly.RuntimeError();
    }
  }

  /**
   * Allocates a new nursery and stores its address in the appropriate
   * field of the StgRegTable of the main capability.
   */
  updateNursery() {
    // Note: the 'rHpAlloc' field of the 'StgRegTable' C struct contains
    // the number of bytes allocated in the heap, or better the number of
    // bytes attempted to being allocated before the heap check fails.
    // Here, we read this field in the hp_alloc variable and
    // use it to determine the size of the newly allocated nursery.
    const base_reg =
        this.symbolTable.addressOf("MainCapability") + rtsConstants.offset_Capability_r,
      hp_alloc = (
        this.memory.i32Load(base_reg + rtsConstants.offset_StgRegTable_rHpAlloc)
      );
    // reset the number of allocated bytes in the nursery
    this.memory.i32Store(
      base_reg + rtsConstants.offset_StgRegTable_rHpAlloc,
      0
    );
    // The address of the new nursery's block descriptor is stored
    // in the 'rCurrentNursery' field of the StgRegTable of the main capability.
    this.memory.i32Store(
      base_reg + rtsConstants.offset_StgRegTable_rCurrentNursery,
      this.heapAlloc.hpAlloc(hp_alloc)
    );
  }

  /**
   * Performs garbage collection, using scheduler Thread State Objects (TSOs) as roots.
   */
  performGC() {
    if (this.yolo || this.heapAlloc.liveSize() < (this.gcThreshold * rtsConstants.mblock_size)) {
      // Garbage collection is skipped. This happens in yolo mode,
      // or when the total number of "live" blocks is below the given threshold
      // (by "live", we mean allocated and not yet freed - see HeapAlloc.liveSize).
      // This avoids a lot of GC invocations
      // (see {@link https://github.com/tweag/asterius/pull/379}).
      this.updateNursery();
      return;
    }
    this.reentrancyGuard.enter(1);

    // Set the current generation number to 1, so that
    // closures are evacuated in the older generation.
    // Also, only major collections for now.
    this.heapAlloc.setGenerationNo(1);

    // Evacuate TSOs
    for (const [_, tso_info] of this.scheduler.tsos) {
      tso_info.addr = this.evacuateClosure(tso_info.addr);
    }

    // Evacuate stable pointers
    for (const [sp, c] of this.stablePtrManager.spt.entries()) {
      this.stablePtrManager.spt.set(sp, this.evacuateClosure(c));
    }

    // Stage the movement of stable pointers.
    // Step 1: Move all the pointers
    // Step 2: Update the pointer -> stablepointer mapping
    // We cannot do this at the same time, since moving the pointer while
    // we walk the ptr2stable map can yield an infinite loop:
    // eg. (ptr:0 stablename: 42) --MOVE--> (ptr:1 stablename:42) --MOVE--> (ptr:2 stablename:42) ...
    let ptr2stableMoved = new Map();
    for (const [ptr, stable] of this.stableNameManager.ptr2stable.entries()) {
      const ptrMoved = this.evacuateClosure(ptr);
      const stableMoved = this.evacuateClosure(stable);
      ptr2stableMoved.set(ptrMoved, stableMoved);
    }
    this.stableNameManager.ptr2stable = ptr2stableMoved;

    // do the rest of the scavenging work
    this.scavengeLoop();

    // update the ret pointer in the complete TSOs
    for (const [_, tso_info] of this.scheduler.tsos) {
      if (tso_info.ret) {
        const tso = tso_info.addr;
        const stackobj = (
          this.memory.i32Load(tso + rtsConstants.offset_StgTSO_stackobj)
        );
        const sp = (
          this.memory.i32Load(stackobj + rtsConstants.offset_StgStack_sp)
        );
        tso_info.ret = (this.memory.i32Load(sp + 4));
      }
    }

    // mark unused blocks
    this.heapAlloc.handleLiveness(this.liveBlocks, this.deadBlocks);
    // set current generation back to 0
    this.heapAlloc.setGenerationNo(0);
    // allocate a new nursery
    this.updateNursery();
    // garbage collect unused JSVals
    this.components.jsvalManager = this.liveJSValManager;
    // cleanup
    this.nonMovedObjects.clear();
    this.liveBlocks.clear();
    this.deadBlocks.clear();
    this.liveJSValManager = new JSValManager(this.components);
    this.reentrancyGuard.exit(1);
  }
}
