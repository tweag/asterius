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
  i128Load(p) { 
      console.log("*** load:");
      // little endian: number with hex digits <0A0B> at address p 
      // get stored as mem[p] = 0B, mem[p+1] = 0A
      let low = this.dataView.getBigUint64(Memory.unTag(p), true);
      console.log("low: ", low);
      let high = this.dataView.getBigUint64(Memory.unTag(p) + 8, true);
      console.log("high: ", high);
      console.log("----");
      return low | (high << BigInt(64));
  }


  i128Store(p, v) {
      console.log("*** store v: ", v);
      // create all 1s of 64 bits.
      const lowmask = (BigInt(1) << BigInt(64)) - BigInt(1);

      // little endian: number with digits <x y> at address p 
      // get stored as mem[p] = y, mem[p+1] = x
      let low = v & lowmask;
      console.log("v & low: " , low);
      this.dataView.setBigUint64(Memory.unTag(p), low, true);

      let high = (v >> BigInt(64)) & lowmask;
      console.log("v & high: " , high);

      let v_recreated = ((high << BigInt(64)) | low);
      console.log("v_recreated: " , v_recreated);
      console.assert(v_recreated == BigInt(v));

      // byte addressed, so +8 = 64-bit
      this.dataView.setBigUint64(Memory.unTag(p) + 8, high, true);
      console.log("---");
  }

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
