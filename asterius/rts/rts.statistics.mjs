export class Statistics {
  constructor(time, gcStatistics) {
    this.time = time;
    this.gcStatistics = gcStatistics;
    this.counters = {
      init_time: 0,
      gc_time: 0,
      num_canceled_GCs: 0,
      num_GCs: 0,
      copied_bytes: 0,
      allocated_mblocks: 0,
      last_gc_started: 0,
      wasm_memory_size: 0,
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

  memoryInUse(bytes) {
    this.counters.wasm_memory_size = bytes;
  }

  /**
   * Traces the end of the initialization of
   * the runtime system (INIT)
   */
  endInit() {
    if (!this.gcStatistics) return;
    this.counters.init_time = this.now();
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
    this.counters.gc_time += this.now() - this.counters.last_gc_started;
    this.counters.num_canceled_GCs += 1;
  }

  /**
   * Trace the end of a garbage collection.
   */
  endGC() {
    if (!this.gcStatistics) return;
    this.counters.gc_time += this.now() - this.counters.last_gc_started;
    this.counters.num_GCs += 1;
  }

  /**
   * 
   */
  copiedBytes(n) {
    if (!this.gcStatistics) return;
    this.counters.copied_bytes += Statistics.force(n);
  }

  /**
   * Trace the allocation of new megablocks.
   * Called from {@link Memory.getMBlocks}
   * @param n The number of newly allocated megablocks
   */
  allocateMBlocks(n) {
    if (!this.gcStatistics) return;
    this.counters.allocated_mblocks += Statistics.force(n);
  }

  /**
   * Show the various statistics gathered, if any.
   */
  show() {
    if (!this.gcStatistics) return;
    const
      counters = this.counters,
      total_time = this.now(),
      gc_time = counters.gc_time,
      mut_time = total_time - gc_time - counters.init_time;
    const fmt = new Intl.NumberFormat("en-US", {useGrouping: true});

    console.log("Garbage Collector Statistics:");
    console.log("  INIT  time", counters.init_time.toFixed(2) + "s");
    console.log("  MUT   time", mut_time.toFixed(2) + "s");
    console.log("  GC    time", gc_time.toFixed(2) + "s");
    console.log("  Total time", total_time.toFixed(2) + "s");
    console.log("  % GC  time", (gc_time / total_time * 100).toFixed(1) + "%");

    console.log("  " + counters.num_GCs, "garbage collections");
    console.log("  " + counters.num_canceled_GCs, "canceled garbage collections");

    console.log("  " + fmt.format(counters.copied_bytes), "bytes copied during GC");
    console.log("  " + counters.allocated_mblocks, "total MBlocks allocated");
    console.log("  " + Math.floor(counters.wasm_memory_size / 1024 / 1024) + "MB total Wasm memory in use");
  }
}
