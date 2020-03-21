import { Memory } from "./rts.memory.mjs";

export class HeapBuilder {
  constructor(syms, hpalloc, m, jsval_mgr) {
    this.symbolTable = syms;
    this.heapAlloc = hpalloc;
    this.memory = m;
    this.jsvalManager = jsval_mgr;
    Object.freeze(this);
  }

  newHaskellByteArray(buf) {
    const p =
      Math.ceil(
        this.heapAlloc.allocatePinned(Math.ceil((buf.byteLength + 31) / 8)) / 16
      ) * 16;
    this.memory.i8View
      .subarray(Memory.unTag(p + 16), Memory.unTag(p + 16) + buf.byteLength)
      .set(new Uint8Array(buf));
    this.memory.i64Store(p, BigInt(this.symbolTable.stg_ARR_WORDS_info));
    this.memory.i64Store(p + 8, BigInt(buf.byteLength));
    return p;
  }

  fromJSArrayBuffer(_i) {
    return this.newHaskellByteArray(this.jsvalManager.getJSVal(_i));
  }

  toJSArrayBuffer(_addr, len) {
    const addr = Memory.unTag(_addr);
    return this.jsvalManager.newJSVal(
      this.memory.buffer.slice(addr, addr + len)
    );
  }
}
