/**
 * @file Implements Node.js-specific functionality.
 */
import fs from "fs";
import { performance } from "perf_hooks";

class Posix {
  constructor(memory, rtsConstants) {
    this.memory = memory;
    this.rtsConstants = rtsConstants;
    this.dirs = new Map();
    this.lastDir = 0;
    Object.seal(this);
  }
  open(f, h, m) {
    return fs.openSync(this.memory.strLoad(f), h, m);
  }
  close(f) {
    fs.closeSync(f);
    return 0;
  }
  stat(f, b) {
    const r = fs.statSync(this.memory.strLoad(f));
    this.memory.i64Store(
      b + this.rtsConstants.offset_stat_mtime,
      Math.trunc(r.mtimeMs)
    );
    this.memory.i64Store(b + this.rtsConstants.offset_stat_size, r.size);
    this.memory.i64Store(b + this.rtsConstants.offset_stat_mode, r.mode);
    this.memory.i64Store(b + this.rtsConstants.offset_stat_dev, r.dev);
    this.memory.i64Store(b + this.rtsConstants.offset_stat_ino, r.ino);
    return 0;
  }
  fstat(f, b) {
    const r = fs.fstatSync(f);
    this.memory.i64Store(
      b + this.rtsConstants.offset_stat_mtime,
      Math.trunc(r.mtimeMs)
    );
    this.memory.i64Store(b + this.rtsConstants.offset_stat_size, r.size);
    this.memory.i64Store(b + this.rtsConstants.offset_stat_mode, r.mode);
    this.memory.i64Store(b + this.rtsConstants.offset_stat_dev, r.dev);
    this.memory.i64Store(b + this.rtsConstants.offset_stat_ino, r.ino);
    return 0;
  }
  opendir(p) {
    const dir = fs.opendirSync(this.memory.strLoad(p));
    this.dirs.set(++this.lastDir, dir);
    return this.lastDir;
  }
  readdir(dirPtr, pDirEnt) {
    const dirent = this.dirs.get(dirPtr).readSync();
    if (dirent) {
      const l = pDirEnt & 0xffffffff;
      const { read, written } = new TextEncoder().encodeInto(
        dirent.name,
        this.memory.i8View.subarray(l, l + 4095)
      );
      if (read !== dirent.name.length)
        throw new WebAssembly.RuntimeError(
          `${dirent.name} exceeded path limit`
        );
      this.memory.i8View[l + written] = 0;
      return pDirEnt;
    } else {
      return 0;
    }
  }
  closedir(dirPtr) {
    this.dirs.delete(dirPtr);
    return 0;
  }
}

export default {
  /**
   * A custom Time interface, used in {@link TimeCBits}.
   */
  Time: {
    /**
     * Returns the current high-resolution timestamp,
     * where 0 represents the start of the node.js process.
     * @returns A [seconds, nanoseconds] Array.
     */
    getCPUTime: () => {
      const ms = performance.now(),
            s = Math.floor(ms / 1000.0),
            ns = Math.floor(ms - s * 1000) * 1000000;
      return [s, ns];
    },
    /**
     * Returns the current high-resolution timestamp,
     * where 0 represents UNIX Epoch.
     * @returns A [seconds, nanoseconds] Array.
     */
    getUnixEpochTime: () => {
      return process.hrtime();
    },
    /**
     * The resolution of the timestamp in nanoseconds
     * (high-resolution, ~~1ns).
     */
    resolution: 1
  },
  posix: Posix
};
