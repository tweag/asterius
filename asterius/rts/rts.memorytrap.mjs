import * as settings from "./rts.settings.mjs";
import { Memory } from "./rts.memory.mjs";

export class MemoryTrap {
  constructor(logger, syms) {
    this.logger = logger;
    this.symbolLookupTable = {};
    for (const[k, v] of Object.entries(syms)) this.symbolLookupTable[v] = k;
    Object.freeze(this);
  }

  showI64(x) { return "0x" + x.toString(16).padStart(8, "0"); }

  trap(p) {
    if (Memory.getTag(p) !== settings.dataTag) {
      const err =
          new WebAssembly.RuntimeError("Invalid address " + this.showI64(p));
      this.logger.logError(err);
      throw err;
    }
  }

  signal(e, t, p, o, v) {
    this.trap(p);
    this.logger.logInfo([
      e, t, p, this.symbolLookupTable[p], o, v, this.symbolLookupTable[v]
    ]);
  }

  loadI8(p, o, v) { this.signal("load", "i8", p, o, v); }

  loadI16(p, o, v) { this.signal("load", "i16", p, o, v); }

  loadI32(p, o, v) { this.signal("load", "i32", p, o, v); }

  loadI64(p, o, v) { this.signal("load", "i64", p, o, v); }

  loadF32(p, o, v) { this.signal("load", "f32", p, o, v); }

  loadF64(p, o, v) { this.signal("load", "f64", p, o, v); }

  storeI8(p, o, v) { this.signal("store", "i8", p, o, v); }

  storeI16(p, o, v) { this.signal("store", "i16", p, o, v); }

  storeI32(p, o, v) { this.signal("store", "i32", p, o, v); }

  storeI64(p, o, v) { this.signal("store", "i64", p, o, v); }

  storeF32(p, o, v) { this.signal("store", "f32", p, o, v); }

  storeF64(p, o, v) { this.signal("store", "f64", p, o, v); }
}
