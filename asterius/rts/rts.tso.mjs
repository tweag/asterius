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
        func: this.symbolTable.stg_returnToStackTop
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

  getTSOid(tso) {
    return this.memory.i32Load(tso + rtsConstants.offset_StgTSO_id);
  }
}
