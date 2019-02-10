class TSO {
  constructor() {
    this.addr = undefined;
    this.ret = undefined;
    this.rstat = undefined;
    Object.seal(this);
  }
}

export class TSOManager {
  constructor() {
    this.tsos = [];
    Object.freeze(this);
  }

  newTSO() { return this.tsos.push(new TSO()) - 1; }

  getTSOaddr(i) { return this.tsos[i].addr; }

  getTSOret(i) { return this.tsos[i].ret; }

  getTSOrstat(i) { return this.tsos[i].rstat; }

  setTSOaddr(i, addr) { this.tsos[i].addr = addr; }

  setTSOret(i, ret) { this.tsos[i].ret = ret; }

  setTSOrstat(i, rstat) { this.tsos[i].rstat = rstat; }
}
