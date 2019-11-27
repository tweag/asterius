export class StablePtrManager {
  constructor() {
    this.spt = new Map();
    this.lasts = [0, 0];
    Object.freeze(this);
  }

  newWithTag(v, tag) {
    const sp = (++this.lasts[tag] << 1) | tag;
    this.spt.set(sp, v);
    return sp;
  }

  newStablePtr(addr) {
    return this.newWithTag(addr, 0);
  }

  deRefStablePtr(sp) {
    return this.spt.get(sp);
  }

  freeStablePtr(sp) {
    this.spt.delete(sp);
  }

  newJSVal(v) {
    return this.newWithTag(v, 1);
  }

  getJSVal(sp) {
    return this.deRefStablePtr(sp);
  }

  freeJSVal(sp) {
    this.freeStablePtr(sp);
  }

  newTmpJSVal(v) {
    return this.newJSVal(v);
  }

  mutTmpJSVal(sp, f) {
    this.spt.set(sp, f(this.spt.get(sp)));
  }

  freezeTmpJSVal(sp) {
    const v = this.spt.get(sp);
    this.spt.delete(sp);
    return v;
  }

  hasStablePtr(sp) {
    return this.spt.has(sp);
  }

  preserveJSVals(sps) {
    // The logic to free JSVals not spotted on the Haskell heap during GC has
    // been commented out for now, due to a use-after-free issue related to
    // Integers (see #320).

    // Disabling GC for JSVal is the easiest mitigation for the issue. When a
    // new generation Integer library replaces the current integer-simple
    // library, we'll recover the logic here.

    // for (const sp of Array.from(this.spt.keys()))
    //   if (sp & 1 && !sps.has(sp)) this.freeJSVal(sp);
  }
}
