export class AsyncFFI {
  constructor() {
    this.last = 1;
    this.results = new Map();
    Object.seal(this);
  }
  register(p) {
    p.then(r => this.results.set(this.last++, r));
  }
  fetch(i) {
    const r = this.results.get(i);
    this.results.delete(i);
    return r;
  }
}
