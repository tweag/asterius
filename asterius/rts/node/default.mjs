/**
 * @file Implements Node.js-specific functionality.
 */
import { performance } from "perf_hooks";

export default {
  /**
   * The Performance Web API.
   */
  Performance: performance,
  /**
   * A custom Time interface, used in {@link TimeCBits}.
   */
  Time: {
    /**
     * Returns the current millisecond timestamp,
     * where 0 represents the start of the node.js process.
     */
    now: () => {
      return performance.now();
    },
    /**
     * Returns the current high-resolution timestamp,
     * where 0 represents UNIX Epoch. The output is a
     * [seconds, nanoseconds] Array.
     */
    time: () => {
      return process.hrtime();
    },
    /**
     * The resolution of the timestamp in nanoseconds
     * (high-resolution, ~~1ns).
     */
    resolution: 1,
  }
};
