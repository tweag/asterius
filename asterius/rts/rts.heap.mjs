import { encodeUTF32 } from "./rts.utf32.mjs";
import { Memory } from "./rts.memory.mjs";

export class Heap {
  constructor(syms, i, m, jsval_mgr) {
    this.symbolTable = syms;
    this.instance = i;
    this.memory = m;
    this.jsvalManager = jsval_mgr;
    Object.seal(this);
  }
  newHaskellList(elem_con_info, s, _last) {
    const last =
        _last ? _last : this.symbolTable.ghczmprim_GHCziTypes_ZMZN_closure + 1;
    if (s.length) {
      const rp = this.instance.exports.allocate(s.length * 5);
      for (let i = 0; i < s.length; ++i) {
        this.memory.i64Store(
            rp + ((i * 5) << 3),
            BigInt(this.symbolTable.ghczmprim_GHCziTypes_ZC_con_info));
        this.memory.i64Store(rp + ((i * 5 + 1) << 3), BigInt(rp + i * 40 + 25));
        this.memory.i64Store(rp + ((i * 5 + 2) << 3),
                             BigInt(rp + (i + 1) * 40 + 2));
        this.memory.i64Store(rp + ((i * 5 + 3) << 3), BigInt(elem_con_info));
        this.memory.i64Store(rp + ((i * 5 + 4) << 3), BigInt(s[i]));
      }
      this.memory.i64Store(rp + (((s.length - 1) * 5 + 2) << 3), BigInt(last));
      return rp + 2;
    } else
      return last;
  }
  newHaskellByteArray(buf) {
    const p = Math.ceil((this.instance.exports.allocate(
                            Math.ceil((buf.byteLength + 31) / 8))) /
                        16) *
              16;
    this.memory.i8View
        .subarray(Memory.unTag64(p + 16), Memory.unTag64(p + 16) + buf.byteLength)
        .set(new Uint8Array(buf));
    this.memory.i64Store(p, BigInt(this.symbolTable.stg_ARR_WORDS_info));
    this.memory.i64Store(p + 8, BigInt(buf.byteLength));
    return p;
  }
  newHaskellString(s, _last) {
    return this.newHaskellList(
        this.symbolTable.ghczmprim_GHCziTypes_Czh_con_info,
        new Uint32Array(encodeUTF32(s)), _last);
  }
  newHaskellJSValList(arr) {
    return this.newHaskellList(
        this.symbolTable.ghczmprim_GHCziTypes_Izh_con_info,
        arr.map(v => this.jsvalManager.newJSVal(v)));
  }
  fromJSArrayBuffer(_i) {
    return this.newHaskellByteArray(this.jsvalManager.getJSVal(_i));
  }
  fromJSString(_i) {
    return this.newHaskellString(this.jsvalManager.getJSVal(_i));
  }
  fromJSArray(_i) {
    return this.newHaskellJSValList(this.jsvalManager.getJSVal(_i));
  }
  toJSArrayBuffer(_addr, len) {
    const addr = Memory.unTag64(_addr);
    return this.jsvalManager.newJSVal(
        this.memory.buffer.slice(addr, addr + len));
  }
}
