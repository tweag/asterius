export class StablePtrManager {
  constructor() {
    this.spt = new Map();
    this.last = 0;
    Object.seal(this);
  }

  newStablePtr(addr) {
    const sp = ++this.last;
    this.spt.set(sp, addr);
    return sp;
  }

  deRefStablePtr(sp) {
    return this.spt.get(sp);
  }

  freeStablePtr(sp) {
    this.spt.delete(sp);
  }

  hasStablePtr(sp) {
    return this.spt.has(sp);
  }
}
