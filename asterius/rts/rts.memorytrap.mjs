import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

export class MemoryTrap {
  constructor(logger, syms) {
    this.logger = logger;
    this.symbolLookupTable = {};
    for (const[k, v] of Object.entries(syms)) this.symbolLookupTable[v] = k;
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

  loadI8(p, o, v) {
    this.logger.logInfo([
      "load", "i8", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  loadI16(p, o, v) {
    this.logger.logInfo([
      "load", "i16", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  loadI32(p, o, v) {
    this.logger.logInfo([
      "load", "i32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  loadI64(p, o, v_lo, v_hi) {
    const v = (BigInt(v_hi) << BigInt(32)) | BigInt(v_lo);
    this.logger.logInfo([
      "load", "i64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  loadF32(p, o, v) {
    this.logger.logInfo([
      "load", "f32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  loadF64(p, o, v) {
    this.logger.logInfo([
      "load", "f64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  storeI8(p, o, v) {
    this.logger.logInfo([
      "store", "i8", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  storeI16(p, o, v) {
    this.logger.logInfo([
      "store", "i16", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  storeI32(p, o, v) {
    this.logger.logInfo([
      "store", "i32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  storeI64(p, o, v_lo, v_hi) {
    const v = (BigInt(v_hi) << BigInt(32)) | BigInt(v_lo);
    this.logger.logInfo([
      "store", "i64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  storeF32(p, o, v) {
    this.logger.logInfo([
      "store", "f32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }

  storeF64(p, o, v) {
    this.logger.logInfo([
      "store", "f64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    this.trap(p, o);
  }
}
