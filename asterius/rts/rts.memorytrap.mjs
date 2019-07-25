import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

export class MemoryTrap {
  constructor(logger, syms, memory, mblock_alloc) {
    this.logger = logger;
    this.symbolLookupTable = {};
    for (const[k, v] of Object.entries(syms)) this.symbolLookupTable[v] = k;
    this.memory = memory;
    this.mblockAlloc = mblock_alloc;
    Object.freeze(this);
  }

  showI64(x) { return "0x" + x.toString(16).padStart(8, "0"); }

  trap(p) {
    const tag = Memory.getTag(p),
      untagged = BigInt(Memory.unTag(p)),
      mblock_no = untagged >> BigInt(Math.log2(rtsConstants.mblock_size)),
      mblock_live = Boolean((this.mblockAlloc.bitset >> mblock_no) & BigInt(1));
    if (tag != rtsConstants.dataTag || !mblock_live) {
      const err = new WebAssembly.RuntimeError(
        "Invalid address " + this.showI64(p)
      );
      this.logger.logError(err);
      throw err;
    }
  }

  loadI8(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i8Load(p);
  }

  loadI16(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i16Load(p);
  }

  loadI32(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i32Load(p);
  }

  loadI64(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i64Load(p);
  }

  loadI32S8(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i32LoadS8(p);
  }

  loadI32U8(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i32LoadU8(p);
  }

  loadI32S16(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i32LoadS16(p);
  }

  loadI32U16(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i32LoadU16(p);
  }

  loadI64S8(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i64LoadS8(p);
  }

  loadI64U8(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i64LoadU8(p);
  }

  loadI64S16(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i64LoadS16(p);
  }

  loadI64U16(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.i64LoadU16(p);
  }

  loadF32(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.f32Load(p);
  }

  loadF64(bp, o) {
    const p = bp + BigInt(o);
    this.trap(p);
    return this.memory.f64Load(p);
  }

  storeI8(bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(p);
    this.memory.i8Store(p, v);
  }

  storeI16(bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(p);
    this.memory.i16Store(p, v);
  }

  storeI32(bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(p);
    this.memory.i32Store(p, v);
  }

  storeI64(bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(p);
    this.memory.i64Store(p, v);
  }

  storeF32(bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(p);
    this.memory.f32Store(p, v);
  }

  storeF64(bp, o, v) {
    const p = bp + BigInt(o);
    this.trap(p);
    this.memory.f64Store(p, v);
  }
}
