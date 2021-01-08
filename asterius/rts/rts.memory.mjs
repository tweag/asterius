import * as rtsConstants from "./rts.constants.mjs";

function checkNullAndTag(p) {
  if (!p) {
    throw new WebAssembly.RuntimeError(`Allocator returned NULL`);
  }
  return p;
}

/**
 * Class acting as the low-level interface to Wasm memory.
 * It mainly provides methods to load/store data in memory
 * (e.g. {@link Memory#i64Load}, {@link Memory#i64Store}),
 * static methods to handle pointer tagging (e.g.
 * {@link Memory#getDynTag}), and a MBlock allocator
 * ({@link Memory#getMBlocks} and {@link Memory#freeMBlocks}).
 */
export class Memory {
  constructor(components) {
    this.components = components;

    /**
     * The underlying Wasm Memory instance.
     * @name Memory#memory
     */
    this.memory = undefined;
    /**
     * The number of MBlock slots reserved for
     * the static part of memory (vs the dynamic part
     * where heap objects are allocated at runtime).
     * The static MBlocks contain the initial compiled
     * Wasm code plus auxiliary static data structures
     * like info tables.
     * @name Memory#staticMBlocks
     */
    this.staticMBlocks = undefined;
    /**
     * Low-level interfaces for reading/writing the contents
     * of {@link Memory#memory}.
     * @name Memory#i8view
     * @name Memory#dataView
     */
    Object.seal(this);
  }

  get i8View() {
    return new Uint8Array(this.memory.buffer);
  }

  get dataView() {
    return new DataView(this.memory.buffer);
  }

  /**
   * Initializes the {@link Memory} object.
   */
  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
  }

  static unDynTag(p) {
    const np = Number(p);
    return np - (np & 7);
  }

  static getDynTag(p) {
    return Number(p) & 7;
  }

  static setDynTag(p, t) {
    const np = Number(p);
    return np - (np & 7) + t;
  }

  i8Load(p) {
    return this.i8View[p];
  }

  i8Store(p, v) {
    this.i8View[p] = Number(v);
  }

  i16Load(p) {
    return this.dataView.getUint16(p, true);
  }

  i16Store(p, v) {
    this.dataView.setUint16(p, Number(v), true);
  }

  i32Load(p) {
    return this.dataView.getUint32(p, true);
  }

  i32Store(p, v) {
    this.dataView.setUint32(p, Number(v), true);
  }

  i64Load(p) {
    return this.dataView.getBigUint64(p, true);
  }

  i64Store(p, v) {
    this.dataView.setBigUint64(p, BigInt(v), true);
  }

  f32Load(p) {
    return this.dataView.getFloat32(p, true);
  }

  f32Store(p, v) {
    this.dataView.setFloat32(p, Number(v), true);
  }

  f64Load(p) {
    return this.dataView.getFloat64(p, true);
  }

  f64Store(p, v) {
    this.dataView.setFloat64(p, Number(v), true);
  }

  i32LoadS8(p) {
    return this.dataView.getInt8(p);
  }

  i32LoadU8(p) {
    return this.dataView.getUint8(p);
  }

  i32LoadS16(p) {
    return this.dataView.getInt16(p, true);
  }

  i32LoadU16(p) {
    return this.dataView.getUint16(p, true);
  }

  i64LoadS8(p) {
    return BigInt(this.dataView.getInt8(p));
  }

  i64LoadU8(p) {
    return BigInt(this.dataView.getUint8(p));
  }

  i64LoadS16(p) {
    return BigInt(this.dataView.getInt16(p, true));
  }

  i64LoadU16(p) {
    return BigInt(this.dataView.getUint16(p, true));
  }

  /**
   * Checks whether the object at address {@param p} is
   * heap-allocated, i.e. whether it resides in the dynamic
   * part of the memory. Used during garbage collection
   * (in {@link GC#evacuateClosure}) to avoid evacuating
   * objects in the static MBlocks.
   */
  heapAlloced(p) {
    return (
      p >= this.staticMBlocks << rtsConstants.mblock_size_log2
    );
  }

  /**
   * Obtains {@param n} MBlocks from {@link Memory#memory}.
   * @returns The memory address at the beginning of the
   *   requested free memory area.
   */
  getMBlocks(n) {
    return checkNullAndTag(
      this.components.exports.aligned_alloc(
        rtsConstants.mblock_size,
        rtsConstants.mblock_size * n
      )
    );
  }

  /**
   * Frees MBlocks starting at address {@param p}.
   */
  freeMBlocks(p) {
    this.components.exports.free(p);
  }

  expose(p, len, t) {
    return new t(this.memory.buffer, p, len);
  }

  strlen(_str) {
    return this.components.exports.strlen(_str);
  }

  strLoad(_str) {
    let p = _str;
    let s = "";
    let i = 0;

    while (1) {
      let c = this.i8View[p + i];
      if (c == 0) {
        return s;
      }
      s += String.fromCharCode(c);
      i++;
    }
  }

  memchr(_ptr, val, num) {
    return this.components.exports.memchr(_ptr, val, num);
  }

  memcpy(_dst, _src, n) {
    return this.components.exports.memcpy(_dst, _src, n);
  }

  memset(_dst, c, n, size = 1) {
    // We only allow 1, 2, 4, 8. Any other size should get a runtime error.
    const ty = {
      1: Uint8Array,
      2: Uint16Array,
      4: Uint32Array,
      8: BigUint64Array,
    };
    const buf = this.expose(_dst, n, ty[size]);

    if (size === 8) {
      // TODO: The conversion BigInt(c) is lossy. Numbers are represented as
      // IEEE754 double precision floating point numbers, for which the maximum
      // (representable) safe integer in JavaScript is (Number.MAX_SAFE_INTEGER
      // = 2^53 - 1).
      buf.fill(BigInt(c));
    } else {
      buf.fill(c);
    }
  }

  memsetFloat32(_dst, c, n) {
    const buf = this.expose(_dst, n, Float32Array);
    buf.fill(c);
  }

  memsetFloat64(_dst, c, n) {
    const buf = this.expose(_dst, n, Float64Array);
    buf.fill(c);
  }
}
