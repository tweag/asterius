import * as settings from "./rts.settings.mjs";
import { Memory } from "./rts.memory.mjs";

export class MemoryTrap {
  constructor(memory, logger, syms) {
    this.memory = memory;
    this.logger = logger;
    this.symbolLookupTable = {};
    for (const[k, v] of Object.entries(syms)) this.symbolLookupTable[v] = k;
    Object.freeze(this);
  }

  showI64(x) { return "0x" + x.toString(16).padStart(8, "0"); }

  trap(_p, o) {
    const p = _p + o;
    if (Memory.getTag(p) != settings.dataTag) {
      const err =
          new WebAssembly.RuntimeError("Invalid address " + this.showI64(p));
      this.logger.logError(err);
      throw err;
    }
  }

  loadI8(p, o) {
    this.trap(p, o);
    const v = this.memory.i8Load(p + o);
    this.logger.logInfo([
      "load", "i8", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    return v;
  }

  loadI16(p, o) {
    this.trap(p, o);
    const v = this.memory.i16Load(p + o);
    this.logger.logInfo([
      "load", "i16", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    return v;
  }

  loadI32(p, o) {
    this.trap(p, o);
    const v = this.memory.i32Load(p + o);
    this.logger.logInfo([
      "load", "i32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    return v;
  }

  loadI64(p, o) {
    this.trap(p, o);
    const v = this.memory.i64Load(p + o);
    this.logger.logInfo([
      "load", "i64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    return Number(v);
  }

  loadF32(p, o) {
    this.trap(p, o);
    const v = this.memory.f32Load(p + o);
    this.logger.logInfo([
      "load", "f32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    return v;
  }

  loadF64(p, o) {
    this.trap(p, o);
    const v = this.memory.f64Load(p + o);
    this.logger.logInfo([
      "load", "f64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
    return v;
  }

  storeI8(p, o, v) {
    this.trap(p, o);
    this.memory.i8Store(p + o, v);
    this.logger.logInfo([
      "store", "i8", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
  }

  storeI16(p, o, v) {
    this.trap(p, o);
    this.memory.i16Store(p + o, v);
    this.logger.logInfo([
      "store", "i16", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
  }

  storeI32(p, o, v) {
    this.trap(p, o);
    this.memory.i32Store(p + o, v);
    this.logger.logInfo([
      "store", "i32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
  }

  storeI64(p, o, v) {
    this.trap(p, o);
    this.memory.i64Store(p + o, v);
    this.logger.logInfo([
      "store", "i64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
  }

  storeF32(p, o, v) {
    this.trap(p, o);
    this.memory.f32Store(p + o, v);
    this.logger.logInfo([
      "store", "f32", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
  }

  storeF64(p, o, v) {
    this.trap(p, o);
    this.memory.f64Store(p + o, v);
    this.logger.logInfo([
      "store", "f64", p, this.symbolLookupTable[p], o, v,
      this.symbolLookupTable[v]
    ]);
  }
}
