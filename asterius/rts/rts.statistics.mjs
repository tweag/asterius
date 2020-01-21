/**
 * Class for gathering various statistics
 * about the runtime system.
 */
export class Statistics {
  /**
   * @param time The target-specific Time module
   * @param gcStatistics Whether to gather statistics about GC
   */
  constructor(time, gcStatistics) {
    this.time = time;
    if (gcStatistics)
      this.gcStatistics = {
        init_time: 0,
        gc_time: 0,
        num_canceled_GCs: 0,
        num_GCs: 0,
        copied_bytes: 0,
        allocated_mblocks: 0,
        last_gc_started: 0,
        wasm_memory_size: 0
      };
    Object.freeze(this);
  }

  /**
   * Forces the argument to be evaluated, i.e.
   * in case it is a callback, then it is called
   * and the result returned. Useful when tracing/logging
   * data that is expensive to compute, so that the actual
   * value is computed only if the logging level is
   * appropriate.
   * @param arg The argument to force
   * @private
   */
  static force(arg) {
    if (typeof arg === "function") {
      return arg();
    } else {
      return arg;
    }
  }

  /**
   * Returns the CPU time in seconds.
   * @private
   */
  now() {
    const [s, ns] = this.time.getCPUTime();
    return s + ns / 1000000000;
  }

  /**
   * Traces the current allocate Wasm memory.
   * @param bytes The size of the Wasm memory in bytes
   */
  memoryInUse(bytes) {
    if (!this.gcStatistics) return;
    this.gcStatistics.wasm_memory_size = Math.max(
      this.gcStatistics.wasm_memory_size,
      bytes
    );
  }

  /**
   * Traces the end of the initialization of
   * the runtime system (INIT)
   */
  endInit() {
    if (!this.gcStatistics) return;
    this.gcStatistics.init_time = this.now();
  }

  /**
   * Traces the start of a garbage collection
   * i.e. when {@link GC#performGC} is called.
   */
  startGC() {
    if (!this.gcStatistics) return;
    this.gcStatistics.last_gc_started = this.now();
  }

  /**
   * Traces a canceled garbage collection.
   */
  cancelGC() {
    if (!this.gcStatistics) return;
    this.gcStatistics.gc_time += this.now() - this.gcStatistics.last_gc_started;
    this.gcStatistics.num_canceled_GCs += 1;
  }

  /**
   * Traces the end of a garbage collection.
   */
  endGC() {
    if (!this.gcStatistics) return;
    this.gcStatistics.gc_time += this.now() - this.gcStatistics.last_gc_started;
    this.gcStatistics.num_GCs += 1;
  }

  /**
   * Traces the number of bytes copied during
   * GC due to evacuation. Called from {@link GC#copyClosure}.
   * @param n The number of copied bytes
   */
  copiedBytes(n) {
    if (!this.gcStatistics) return;
    this.gcStatistics.copied_bytes += Statistics.force(n);
  }

  /**
   * Traces the allocation of new MBlocks.
   * Called from {@link Memory#getMBlocks}
   * @param n The number of newly allocated MBlocks
   */
  allocateMBlocks(n) {
    if (!this.gcStatistics) return;
    this.gcStatistics.allocated_mblocks += Statistics.force(n);
  }

  /**
   * Shows the various statistics gathered, if any.
   */
  show() {
    if (!this.gcStatistics) return;
    const counters = this.gcStatistics,
      total_time = this.now(),
      gc_time = counters.gc_time,
      mut_time = total_time - gc_time - counters.init_time;
    const fmt = new Intl.NumberFormat("en-US", { useGrouping: true });

    console.log("Garbage Collector Statistics:");
    console.log("  INIT  time", counters.init_time.toFixed(2) + "s");
    console.log("  MUT   time", mut_time.toFixed(2) + "s");
    console.log("  GC    time", gc_time.toFixed(2) + "s");
    console.log("  Total time", total_time.toFixed(2) + "s");
    console.log(
      "  % GC  time",
      ((gc_time / total_time) * 100).toFixed(1) + "%"
    );

    console.log("  " + counters.num_GCs, "garbage collections");
    console.log(
      "  " + counters.num_canceled_GCs,
      "canceled garbage collections"
    );

    console.log(
      "  " + fmt.format(counters.copied_bytes),
      "bytes copied during GC"
    );
    console.log("  " + counters.allocated_mblocks, "total MBlocks allocated");
    console.log(
      "  " +
        Math.floor(counters.wasm_memory_size / 1024 / 1024) +
        "MB total Wasm memory in use"
    );
  }
}
