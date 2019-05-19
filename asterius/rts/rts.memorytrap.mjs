import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

export class MemoryTrap {
  constructor(logger, syms, memory) {
    this.logger = logger;
    this.symbolLookupTable = {};
    for (const[k, v] of Object.entries(syms)) this.symbolLookupTable[v] = k;
    this.memory = memory;
    Object.freeze(this);
  }

  showI64(x) { return "0x" + x.toString(16).padStart(8, "0"); }

  trap(_p, o) {
    const p = _p + o;
    if (Memory.getTag(p) != rtsConstants.dataTag) {
      const err =
          new WebAssembly.RuntimeError("Invalid address " + this.showI64(p));
      this.logger.logError(err);
      throw err;
    }
  }

  loadI8(bp, o) {
    /*
    this.logger.logInfo([
      "load", "i8", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    return this.memory.i8Load(p);
  }

  loadI16(bp, o) {
    /*
    this.logger.logInfo([
      "load", "i16", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    return this.memory.i16Load(p);
  }

  loadI32(bp, o) {
    /*
    this.logger.logInfo([
      "load", "i32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    return this.memory.i32Load(p);
  }

  loadI64(bp, o) {
    /*
    const v = (BigInt(v_hi) << BigInt(32)) | BigInt(v_lo);
    this.logger.logInfo([
      "load", "i64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    return this.memory.i64Load(p);
  }

  loadI32S8(bp, o) {
    const p = bp + BigInt(o);
    return this.memory.i32LoadS8(p);
  }

  loadI32U8(bp, o) {
    const p = bp + BigInt(o);
    return this.memory.i32LoadU8(p);
  }

  loadI32S16(bp, o) {
    const p = bp + BigInt(o);
    return this.memory.i32LoadS16(p);
  }

  loadI32U16(bp, o) {
    const p = bp + BigInt(o);
    return this.memory.i32LoadU16(p);
  }

  loadI64S8(bp, o) {
    const p = bp + BigInt(o);
    return this.memory.i64LoadS8(p);
  }

  loadI64U8(bp, o) {
    const p = bp + BigInt(o);
    return this.memory.i64LoadU8(p);
  }

  loadI64S16(bp, o) {
    const p = bp + BigInt(o);
    return this.memory.i64LoadS16(p);
  }

  loadI64U16(bp, o) {
    const p = bp + BigInt(o);
    return this.memory.i64LoadU16(p);
  }

  loadF32(bp, o) {
    /*
    this.logger.logInfo([
      "load", "f32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    return this.memory.f32Load(p);
  }

  loadF64(bp, o) {
    /*
    this.logger.logInfo([
      "load", "f64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    return this.memory.f64Load(p);
  }

  storeI8(bp, o, v) {
    /*
    this.logger.logInfo([
      "store", "i8", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    this.memory.i8Store(p, v);
  }

  storeI16(bp, o, v) {
    /*
    this.logger.logInfo([
      "store", "i16", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    this.memory.i16Store(p, v);
  }

  storeI32(bp, o, v) {
    /*
    this.logger.logInfo([
      "store", "i32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    this.memory.i32Store(p, v);
  }

  storeI64(bp, o, v) {
    /*
    const v = (BigInt(v_hi) << BigInt(32)) | BigInt(v_lo);
    this.logger.logInfo([
      "store", "i64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    this.memory.i64Store(p, v);
  }

  storeF32(bp, o, v) {
    /*
    this.logger.logInfo([
      "store", "f32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    this.memory.f32Store(p, v);
  }

  storeF64(bp, o, v) {
    /*
    this.logger.logInfo([
      "store", "f64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
    */
    const p = bp + BigInt(o);
    this.memory.f64Store(p, v);
  }
}
