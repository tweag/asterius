export class StablePtrManager {
  constructor() {
    this.spt = new Map();
    this.rev_spt = new Map();
    this.last = 0;
    Object.seal(this);
  }

  newWithTag(v, tag) {
    let sp = this.rev_spt.get(v);
    if (sp === undefined) {
      sp = (++this.last << 1) | tag;
      this.spt.set(sp, v);
      this.rev_spt.set(v, sp);
    }
    return sp;
  }

  newStablePtr(addr) { return this.newWithTag(addr, 0); }

  deRefStablePtr(sp) { return this.spt.get(sp); }

  freeStablePtr(sp) {
    this.rev_spt.delete(this.spt.get(sp));
    this.spt.delete(sp);
  }

  newJSVal(v) { return this.newWithTag(v, 1); }

  getJSVal(sp) { return this.deRefStablePtr(sp); }

  freeJSVal(sp) { this.freeStablePtr(sp); }

  newTmpJSVal(v) {
    const sp = (++this.last << 1) | 1;
    this.spt.set(sp, v);
    return sp;
  }

  mutTmpJSVal(sp, f) { this.spt.set(sp, f(this.spt.get(sp))); }

  freezeTmpJSVal(sp) {
    const v = this.spt.get(sp);
    this.spt.delete(sp);
    return v;
  }
}
