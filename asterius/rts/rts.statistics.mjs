export class Statistics {
  constructor(time, gcStatistics) {
    this.time = time;
    this.gcStatistics = gcStatistics;
    this.counters = {
      init_wall_s: 0,
      gc_wall_s: 0,
      num_canceled_GCs: 0,
      num_GCs: 0,
      copiedMBlocks: [],
      allocatedMBlocks: 0,
      last_gc_started: 0,
      wasm_memory_capacity: 0,
    };
    Object.freeze(this);
  }

  /**
   * Force @param arg to be evaluated, i.e.
   * in case it is a callback, @param arg is called
   * and the result returned. Useful when tracing/logging
   * data that is expensive to compute, so that the actual
   * value is computed only if the logging level is
   * appropriate.
   */
  static force(arg) {
    if (typeof arg === "function") {
      return arg();
    } else {
      return arg;
    }
  }

  now() {
    const [s,ns] = this.time.getCPUTime();
    return s + (ns / 1000000000);
  }

  memoryCapacity(bytes) {
    this.counters.wasm_memory_capacity = bytes;
  }

  /**
   * Traces the end of the initialization of
   * the runtime system (INIT)
   */
  endInit() {
    if (!this.gcStatistics) return;
    this.counters.init_wall_s = this.now();
  }

  /**
   * Traces the request of a garbage collection
   * i.e. {@link GC.performGC}.
   */
  startGC() {
    if (!this.gcStatistics) return;
    this.counters.last_gc_started = this.now();
  }

  /**
   * Trace a cancelled garbage collection.
   */
  cancelGC() {
    if (!this.gcStatistics) return;
    this.counters.gc_wall_s += this.now() - this.counters.last_gc_started;
    this.counters.num_canceled_GCs += 1;
  }

  /**
   * Trace the end of a garbage collection.
   */
  endGC() {
    if (!this.gcStatistics) return;
    this.counters.gc_wall_s += this.now() - this.counters.last_gc_started;
    this.counters.num_GCs += 1;
  }

  /**
   * 
   */
  copiedMBlocks(n) {
    if (!this.gcStatistics) return;
    n = Statistics.force(n);
    this.counters.copiedMBlocks.push(n);
  }

  /**
   * Trace the allocation of new megablocks.
   * Called from {@link Memory.getMBlocks}
   * @param n The number of newly allocated megablocks
   */
  getMBlocks(n) {
    if (!this.gcStatistics) return;
    this.counters.allocatedMBlocks += Statistics.force(n);
  }

  /**
   * Display various GC statistics.
   */
  displayGCStatistics() {
    if (!this.gcStatistics) return;
    var counters = this.counters;
    var total_wall_s = this.now();
    var gc_wall_s = counters.gc_wall_s;
    var mutator_wall_s = total_wall_s - gc_wall_s - counters.init_wall_s;
    var totalCopiedMBlocksNo = 0;
    for(const n of counters.copiedMBlocks) {
      totalCopiedMBlocksNo += n;
    }
    var copiedMBlocksAverageNo = "n/a";
    if (counters.copiedMBlocks.length != 0) 
      copiedMBlocksAverageNo = totalCopiedMBlocksNo / counters.copiedMBlocks.length;
    console.log("Garbage Collector Statistics", {
      init_wall_seconds: counters.init_wall_s,
      mutator_wall_seconds: mutator_wall_s,
      GC_wall_seconds: gc_wall_s,
      num_GCs: counters.num_GCs,
      num_canceled_GCs: counters.num_canceled_GCs,
      copied_mblocks: totalCopiedMBlocksNo,
      average_copied_mblocks: copiedMBlocksAverageNo,
      allocated_mblocks: counters.allocatedMBlocks,
      wasm_memory_capacity: counters.wasm_memory_capacity
    });
  }
}
