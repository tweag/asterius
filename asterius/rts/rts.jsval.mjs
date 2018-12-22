export class JSValManager {
  constructor() {
    this.vals = [];
    this.revs = new Map();
    this.tmpvals = [];
    Object.seal(this);
  }

  newJSVal(v) {
    let i = this.revs.get(v);
    if (i === undefined) {
      i = this.vals.push(v) - 1;
      this.revs.set(v, i);
    }
    return i;
  }

  getJSVal(i) { return this.vals[i]; }

  newTmpJSVal(v) { return this.tmpvals.push(v) - 1; }

  mutTmpJSVal(i, f) { this.tmpvals[i] = f(this.tmpvals[i]); }

  freezeTmpJSVal(i) {
    const v = this.tmpvals[i];
    delete this.tmpvals[i];
    return v;
  }
};
