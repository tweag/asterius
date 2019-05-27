import * as rtsConstants from "./rts.constants.mjs";

export class Memory {
  constructor() {
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.i8View = undefined;
    this.dataView = undefined;
    Object.seal(this);
  }
  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.initView();
  }
  initView() {
    this.i8View = new Uint8Array(this.memory.buffer);
    this.dataView = new DataView(this.memory.buffer);
  }
  static unTag(p) { return Number(BigInt(p) & BigInt(0xffffffff)); }
  static getTag(p) { return Number(BigInt(p) >> BigInt(32)); }
  static tagData(p) {
    return Number((BigInt(rtsConstants.dataTag) << BigInt(32)) | BigInt(p));
  }
  static tagFunction(p) {
    return Number((BigInt(rtsConstants.functionTag) << BigInt(32)) | BigInt(p));
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
  i16Load(p) { return this.dataView.getUint16(Memory.unTag(p), true); }
  i16Store(p, v) { this.dataView.setUint16(Memory.unTag(p), Number(v), true); }
  i32Load(p) { return this.dataView.getUint32(Memory.unTag(p), true); }
  i32Store(p, v) { this.dataView.setUint32(Memory.unTag(p), Number(v), true); }
  i64Load(p) { return this.dataView.getBigUint64(Memory.unTag(p), true); }
  i64Store(p, v) { this.dataView.setBigUint64(Memory.unTag(p), BigInt(v), true); }
  f32Load(p) { return this.dataView.getFloat32(Memory.unTag(p), true); }
  f32Store(p, v) { this.dataView.setFloat32(Memory.unTag(p), Number(v), true); }
  f64Load(p) { return this.dataView.getFloat64(Memory.unTag(p), true); }
  f64Store(p, v) { this.dataView.setFloat64(Memory.unTag(p), Number(v), true); }
  i32LoadS8(p) { return this.dataView.getInt8(Memory.unTag(p)); }
  i32LoadU8(p) { return this.dataView.getUint8(Memory.unTag(p)); }
  i32LoadS16(p) { return this.dataView.getInt16(Memory.unTag(p), true); }
  i32LoadU16(p) { return this.dataView.getUint16(Memory.unTag(p), true); }
  i64LoadS8(p) { return BigInt(this.dataView.getInt8(Memory.unTag(p))); }
  i64LoadU8(p) { return BigInt(this.dataView.getUint8(Memory.unTag(p))); }
  i64LoadS16(p) { return BigInt(this.dataView.getInt16(Memory.unTag(p), true)); }
  i64LoadU16(p) { return BigInt(this.dataView.getUint16(Memory.unTag(p), true)); }
  heapAlloced(p) { return Memory.unTag(p) >= (this.staticMBlocks << Math.log2(rtsConstants.mblock_size)); }
  strlen(_str) { return this.i8View.subarray(Memory.unTag(_str)).indexOf(0); }
  strLoad(_str) {
      let p = Memory.unTag(_str);
      let s = "";
      let i = 0;
      // NOTE: passing a non-null terminated string will give you garbage,
      // as you deserve.
      while(1) {
          let code = this.i8View[p + i];
          if (code == 0) { return s; }
          s += String.fromCharCode(code);
          i += 1;
      }
  }
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
