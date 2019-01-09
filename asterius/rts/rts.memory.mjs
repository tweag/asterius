import * as settings from "./rts.settings.mjs";

export class Memory {
  constructor() {
    this.memory = null;
    Object.seal(this);
  }
  init(memory) {
    this.memory = memory;
  }
  static unTag(p) { return p & 0xffffffff; }
  static getTag(p) { return Number(BigInt(p) >> BigInt(32)); }
  static tagData(p) {
    return Number((BigInt(settings.dataTag) << BigInt(32)) | BigInt(p));
  }
  static tagFunction(p) {
    return Number((BigInt(settings.functionTag) << BigInt(32)) | BigInt(p));
  }
  get buffer() { return this.memory.buffer; }
  grow(n) { return this.memory.grow(n); }
  strlen(_str) {
    return new Uint8Array(this.memory.buffer, Memory.unTag(_str)).indexOf(0);
  }
  memchr(_ptr, val, num) {
    const off = new Uint8Array(this.memory.buffer, Memory.unTag(_ptr), num)
                    .indexOf(val);
    return off === -1 ? 0 : _ptr + off;
  }
  memcpy(_dst, _src, n) {
    new Uint8Array(this.memory.buffer)
        .copyWithin(Memory.unTag(_dst), Memory.unTag(_src),
                    Memory.unTag(_src) + n);
  }
  memmove(_dst, _src, n) { return this.memcpy(_dst, _src, n); }
  memset(_dst, c, n) {
    new Uint8Array(this.memory.buffer)
        .fill(c, Memory.unTag(_dst), Memory.unTag(_dst) + n);
  }
  memcmp(_ptr1, _ptr2, n) {
    const buf = new Uint8Array(this.memory.buffer);
    for (let i = 0; i < n; ++i) {
      const sgn = Math.sign(buf[Memory.unTag(_ptr1) + i] -
                            buf[Memory.unTag(_ptr2) + i]);
      if (sgn) return sgn;
    }
    return 0;
  }
}
