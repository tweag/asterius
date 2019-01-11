import * as settings from "./rts.settings.mjs";

export class Memory {
  constructor() {
    this.memory = null;
    this.i8View = null;
    this.i64View = null;
    Object.seal(this);
  }
  init(memory) {
    this.memory = memory;
    this.initView();
  }
  initView() {
    this.i8View = new Uint8Array(this.memory.buffer);
    this.i64View = new BigUint64Array(this.memory.buffer);
  }
  static unTag(p) { return Number(BigInt(p) & BigInt(0xfffffff8)); }
  static getTag(p) { return Number(BigInt(p) >> BigInt(32)); }
  static tagData(p) {
    return Number((BigInt(settings.dataTag) << BigInt(32)) | BigInt(p));
  }
  static tagFunction(p) {
    return Number((BigInt(settings.functionTag) << BigInt(32)) | BigInt(p));
  }
  static getDynTag(p) { return p & 7; }
  get buffer() { return this.memory.buffer; }
  grow(n) {
    const prev_pages = this.memory.grow(n);
    this.initView();
    return prev_pages;
  }
  i64Load(p) { return this.i64View[Memory.unTag(p) >> 3]; }
  i64Store(p, v) { this.i64View[Memory.unTag(p) >> 3] = v; }
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
