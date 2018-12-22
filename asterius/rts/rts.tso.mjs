class TSO {
  constructor() {
    this.ret = undefined;
    this.rstat = undefined;
    Object.seal(this);
  }
}

export class TSOManager {
  constructor() {
    this.tsos = [];
    Object.seal(this);
  }

  newTSO() { return this.tsos.push(new TSO()) - 1; }

  getTSOret(i) { return this.tsos[i].ret; }

  getTSOrstat(i) { return this.tsos[i].rstat; }

  setTSOret(i, ret) { this.tsos[i].ret = ret; }

  setTSOrstat(i, rstat) { this.tsos[i].rstat = rstat; }
}
