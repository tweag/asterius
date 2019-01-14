export class StablePtrManager {
  constructor() {
    this.spt = new Map();
    this.rev_spt = new Map();
    Object.freeze(this);
  }

  newStablePtr(addr) {
    let sp = this.rev_spt.get(addr);
    if (!sp) {
      sp = this.spt.size + 1;
      this.spt.set(sp, addr);
      this.rev_spt.set(addr, sp);
    }
    return sp;
  }

  deRefStablePtr(sp) { return this.spt.get(sp); }

  freeStablePtr(sp) {
    this.rev_spt.delete(this.spt.get(sp));
    this.spt.delete(sp);
  }
}
