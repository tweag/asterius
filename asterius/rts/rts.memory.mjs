import * as settings from "./rts.settings.mjs";

export class Memory {
  constructor() {
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.i8View = undefined;
    this.i16View = undefined;
    this.i32View = undefined;
    this.i64View = undefined;
    this.f32View = undefined;
    this.f64View = undefined;
    Object.seal(this);
  }
  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.initView();
  }
  initView() {
    this.i8View = new Uint8Array(this.memory.buffer);
    this.i16View = new Uint16Array(this.memory.buffer);
    this.i32View = new Uint32Array(this.memory.buffer);
    this.i64View = new BigUint64Array(this.memory.buffer);
    this.f32View = new Float32Array(this.memory.buffer);
    this.f64View = new Float64Array(this.memory.buffer);
  }
  static unTag(p) { return Number(BigInt(p) & BigInt(0xffffffff)); }
  static getTag(p) { return Number(BigInt(p) >> BigInt(32)); }
  static tagData(p) {
    return Number((BigInt(settings.dataTag) << BigInt(32)) | BigInt(p));
  }
  static tagFunction(p) {
    return Number((BigInt(settings.functionTag) << BigInt(32)) | BigInt(p));
  }
  static unDynTag(p) { return Number((BigInt(p) >> BigInt(3)) << BigInt(3)); }
  static getDynTag(p) { return Number(BigInt(p) & BigInt(7)); }
  static setDynTag(p, t) { return Number(BigInt(p) | BigInt(t)); }
  get buffer() { return this.memory.buffer; }
  grow(n) {
    const prev_pages = this.memory.grow(n);
    this.initView();
    return prev_pages;
  }
  i8Load(p) { return this.i8View[Memory.unTag(p)]; }
  i8Store(p, v) { this.i8View[Memory.unTag(p)] = Number(v); }
  i16Load(p) { return this.i16View[Memory.unTag(p) >> 1]; }
  i16Store(p, v) { this.i16View[Memory.unTag(p) >> 1] = Number(v); }
  i32Load(p) { return this.i32View[Memory.unTag(p) >> 2]; }
  i32Store(p, v) { this.i32View[Memory.unTag(p) >> 2] = Number(v); }
  i64Load(p) { return this.i64View[Memory.unTag(p) >> 3]; }
  i64Store(p, v) { this.i64View[Memory.unTag(p) >> 3] = BigInt(v); }
  f32Load(p) { return this.f32View[Memory.unTag(p) >> 2]; }
  f32Store(p, v) { this.f32View[Memory.unTag(p) >> 2] = Number(v); }
  f64Load(p) { return this.f64View[Memory.unTag(p) >> 3]; }
  f64Store(p, v) { this.f64View[Memory.unTag(p) >> 3] = Number(v); }
  heapAlloced(p) { return Memory.unTag(p) >= (this.staticMBlocks << Math.log2(settings.mblock_size)); }
  strlen(_str) { return this.i8View.subarray(Memory.unTag(_str)).indexOf(0); }
  memchr(_ptr, val, num) {
    const ptr = Memory.unTag(_ptr),
          off = this.i8View.subarray(ptr, ptr + num).indexOf(val);
    return off === -1 ? 0 : _ptr + off;
  }
  memcpy(_dst, _src, n) {
    this.i8View.copyWithin(Memory.unTag(_dst), Memory.unTag(_src),
                           Memory.unTag(_src) + n);
  }
  memmove(_dst, _src, n) { return this.memcpy(_dst, _src, n); }
  memset(_dst, c, n) {
    this.i8View.fill(c, Memory.unTag(_dst), Memory.unTag(_dst) + n);
  }
  memcmp(_ptr1, _ptr2, n) {
    for (let i = 0; i < n; ++i) {
      const sgn = Math.sign(this.i8View[Memory.unTag(_ptr1) + i] -
                            this.i8View[Memory.unTag(_ptr2) + i]);
      if (sgn) return sgn;
    }
    return 0;
  }
}
