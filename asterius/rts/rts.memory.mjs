import * as settings from "./rts.settings.mjs";

export class Memory {
  constructor() {
    this.memory = undefined;
    this.instance = undefined;
    this.staticMBlocks = undefined;
    this.i8View = undefined;
    Object.seal(this);
  }
  init(memory, instance, static_mblocks) {
    this.memory = memory;
    this.instance = instance;
    this.staticMBlocks = static_mblocks;
    this.initView();
  }
  initView() {
    this.i8View = new Uint8Array(this.memory.buffer);
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
  i16Load(p) { return this.instance.exports.__asterius_raw_load_i16(Number(p)); }
  i16Store(p, v) { this.instance.exports.__asterius_raw_store_i16(Number(p), Number(v)); }
  i32Load(p) { return this.instance.exports.__asterius_raw_load_i32(Number(p)); }
  i32Store(p, v) { this.instance.exports.__asterius_raw_store_i32(Number(p), Number(v)); }
  i64Load(p) { return (BigInt(this.instance.exports.__asterius_raw_load_i32(Number(p) + 4)) << BigInt(32)) | BigInt(this.instance.exports.__asterius_raw_load_i32(Number(p))); }
  i64Store(p, v) { this.instance.exports.__asterius_raw_store_i32(Number(p), Number(BigInt(v) & BigInt(0xFFFFFFFF))); this.instance.exports.__asterius_raw_store_i32(Number(p)+4, Number(BigInt(v) >> BigInt(32))); }
  f32Load(p) { return this.instance.exports.__asterius_raw_load_f32(Number(p)); }
  f32Store(p, v) { this.instance.exports.__asterius_raw_store_f32(Number(p), Number(v)); }
  f64Load(p) { return this.instance.exports.__asterius_raw_load_f64(Number(p)); }
  f64Store(p, v) { this.instance.exports.__asterius_raw_store_f64(Number(p), Number(v)); }
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
