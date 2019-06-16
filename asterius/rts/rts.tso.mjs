import { Memory } from "./rts.memory.mjs";
import * as rtsConstants from "./rts.constants.mjs";

export class TSOManager {
  constructor(memory, symbol_table) {
    this.memory = memory;
    this.symbolTable = symbol_table;
    this.last = 0;
    this.tsos = new Map();
    Object.seal(this);
  }

  newTSO() {
    const tid = ++this.last;
    this.tsos.set(
      tid,
      Object.seal({
        addr: -1,
        ret: -1,
        rstat: -1,
        func: this.symbolTable.stg_returnToStackTop,
        regs: undefined,
        result: undefined,
        reject: undefined
      })
    );
    return tid;
  }

  getTSOaddr(i) {
    return this.tsos.get(i).addr;
  }

  getTSOret(i) {
    return this.tsos.get(i).ret;
  }

  getTSOrstat(i) {
    return this.tsos.get(i).rstat;
  }

  getTSOfunc(i) {
    return this.tsos.get(i).func;
  }

  getTSOregs(i) {
    this.memory.i8View.set(
      this.tsos.get(i).regs,
      Memory.unTag(this.symbolTable.__asterius_regs)
    );
  }

  getTSOresult(i) {
    return this.tsos.get(i).result;
  }

  getTSOreject(i) {
    return this.tsos.get(i).reject;
  }

  setTSOaddr(i, addr) {
    this.tsos.get(i).addr = addr;
  }

  setTSOret(i, ret) {
    this.tsos.get(i).ret = ret;
  }

  setTSOrstat(i, rstat) {
    this.tsos.get(i).rstat = rstat;
  }

  setTSOfunc(i, func) {
    this.tsos.get(i).func = func;
  }

  setTSOregs(i) {
    const p = Memory.unTag(this.symbolTable.__asterius_regs),
      tso = this.tsos.get(i);
    if (!tso.regs) tso.regs = new Uint8Array(1024);
    tso.regs.set(this.memory.i8View.subarray(p, p + 1024));
  }

  setTSOpromise(i, p) {
    p.then(
      r => (this.tsos.get(i).result = r),
      err => (this.tsos.get(i).reject = err)
    );
  }

  getTSOid(tso) {
    return this.memory.i32Load(tso + rtsConstants.offset_StgTSO_id);
  }
}
