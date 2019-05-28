export class ReentrancyGuard {
  constructor(names) {
    this.names = names;
    this.flags = this.names.map(() => false);
    Object.freeze(this);
  }

  enter(i) {
    if (this.flags[i])
      throw new WebAssembly.RuntimeError(
        `ReentrancyGuard: ${this.names[i]} reentered!`
      );
    this.flags[i] = true;
  }

  exit(i) {
    this.flags[i] = false;
  }
}
