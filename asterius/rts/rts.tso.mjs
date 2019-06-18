import { Memory } from "./rts.memory.mjs";
import * as rtsConstants from "./rts.constants.mjs";

class TSO {
  constructor() {
    this.addr = undefined;
    this.ret = undefined;
    this.rstat = undefined;
  }
}

export class TSOManager {
  constructor(memory, symbol_table) {
    this.memory = memory;
    this.symbolTable = symbol_table;
    this.last = 0;
    this.tsos = new Map();
    Object.seal(this);
  }

  newTSO() { return this.tsos.push(new TSO()) - 1; }

  getTSOaddr(i) { return this.tsos[i].addr; }

  getTSOret(i) { return this.tsos[i].ret; }

  getTSOrstat(i) { return this.tsos[i].rstat; }

  setTSOaddr(i, addr) { this.tsos[i].addr = addr; }

  setTSOret(i, ret) { this.tsos[i].ret = ret; }

  setTSOrstat(i, rstat) { this.tsos[i].rstat = rstat; }

  getTSOid(tso) {
    return this.memory.i32Load(tso + rtsConstants.offset_StgTSO_id);
  }
}
