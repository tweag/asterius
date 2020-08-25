/**
 * @file Implements browser-specific functionality.
 */

class Posix {
  constructor(memory, rtsConstants) {
    this.memory = memory;
    Object.seal(this);
  }
  getProgArgv(argc, argv_buf) {
    this.memory.i64Store(argc, 1);
  }
  get_errno() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: get_errno");
  }
  set_errno() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: set_errno");
  }
  open() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: open");
  }
  close() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: close");
  }
  ftruncate() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: ftruncate");
  }
  stat() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: stat");
  }
  fstat() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: fstat");
  }
  opendir() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: opendir");
  }
  readdir() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: readdir");
  }
  closedir() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: closedir");
  }
  getenv() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: getenv");
  }
  access() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: access");
  }
  getcwd() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: getcwd");
  }
}

export default {
  /**
   * A custom Time interface, used in {@link TimeCBits}.
   */
  Time: {
    /**
     * Returns the current timestamp, where 0 represents
     * the time origin of the document.
     * @returns A [seconds, nanoseconds] Array.
     */
    getCPUTime: () => {
      const ms = performance.now(),
            s = Math.floor(ms / 1000.0),
            ns = Math.floor(ms - s * 1000) * 1000000;
      return [s, ns];
    },
    /**
     * Returns the current timestamp, where 0 represents UNIX Epoch.
     * @returns A [seconds, nanoseconds] Array.
     */
    getUnixEpochTime: () => {
      const ms = Date.now(),
            s = Math.floor(ms / 1000.0),
            ns = Math.floor(ms - s * 1000) * 1000000;
      return [s, ns];
    },
    /**
     * The resolution of the timestamps in nanoseconds.
     * Note! Due to the Spectre attack, browsers do not
     * provide high-resolution timestamps anymore.
     * See https://developer.mozilla.org/en-US/docs/Web/API/Performance/now
     * and https://spectreattack.com.
     * We fallback to a resolution of 1ms.
     */
    resolution: 1000000
  },
  posix: Posix
};
