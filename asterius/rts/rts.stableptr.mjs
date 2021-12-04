import { isI32 } from "./rts.typecheck.mjs";

export class StablePtrManager {
  constructor() {
    this.spt = new Map();
    this.last = 0;
    Object.seal(this);
  }

  newStablePtr(addr) {
    isI32(addr);
    const sp = ++this.last;
    this.spt.set(sp, addr);
    return isI32(sp);
  }

  deRefStablePtr(sp) {
    isI32(sp);
    return isI32(this.spt.get(sp));
  }

  freeStablePtr(sp) {
    isI32(sp);
    this.spt.delete(sp);
  }

  hasStablePtr(sp) {
    return this.spt.has(sp);
  }
}
