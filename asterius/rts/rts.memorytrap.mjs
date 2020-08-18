import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

function showI64(x) {
  return `0x${x.toString(16)}`;
}

export class MemoryTrap {
  constructor(logger, symbol_table, memory) {
    this.logger = logger;
    this.symbolLookupTable = new Map();
    for (const [k, v] of symbol_table.allEntries()) {
      this.symbolLookupTable.set(v, k);
    }
    this.memory = memory;
    Object.freeze(this);
  }

  trap(sym, p) {
    const tag = Memory.getTag(p),
      untagged = BigInt(Memory.unTag(p)),
      mblock_no = untagged >> BigInt(rtsConstants.mblock_size_log2),
      mblock_live = Boolean((this.memory.liveBitset >> mblock_no) & BigInt(1));
    if (tag != rtsConstants.dataTag || !mblock_live) {
      const err = new WebAssembly.RuntimeError(
        `Invalid address ${showI64(p)} accessed in ${this.symbolLookupTable.get(
          Number(sym)
        )}`
      );
      this.logger.logError(err);
      throw err;
    }
  }

  loadI8(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i8Load(p);
  }

  loadI16(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i16Load(p);
  }

  loadI32(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i32Load(p);
  }

  loadI64(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i64Load(p);
  }

  loadI32S8(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i32LoadS8(p);
  }

  loadI32U8(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i32LoadU8(p);
  }

  loadI32S16(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i32LoadS16(p);
  }

  loadI32U16(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i32LoadU16(p);
  }

  loadI64S8(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i64LoadS8(p);
  }

  loadI64U8(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i64LoadU8(p);
  }

  loadI64S16(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i64LoadS16(p);
  }

  loadI64U16(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.i64LoadU16(p);
  }

  loadF32(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.f32Load(p);
  }

  loadF64(sym, bp, o) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    return this.memory.f64Load(p);
  }

  storeI8(sym, bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    this.memory.i8Store(p, v);
  }

  storeI16(sym, bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    this.memory.i16Store(p, v);
  }

  storeI32(sym, bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    this.memory.i32Store(p, v);
  }

  storeI64(sym, bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    this.memory.i64Store(p, v);
  }

  storeF32(sym, bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    this.memory.f32Store(p, v);
  }

  storeF64(sym, bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(sym, p);
    this.memory.f64Store(p, v);
  }
}
