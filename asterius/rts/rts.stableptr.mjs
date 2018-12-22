export class StablePtrManager {
  constructor() {
    this.spt = [];
    Object.seal(this);
  }

  newStablePtr(addr) { return this.spt.push(addr) - 1; }

  deRefStablePtr(sp) { return this.spt[sp]; }

  freeStablePtr(sp) { delete this.spt[sp]; }
}
