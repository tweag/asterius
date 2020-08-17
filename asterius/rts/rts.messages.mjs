export class Messages {
  constructor(memory, fs) {
    this.memory = memory;
    this.fs = fs;
    this.encoder = new TextEncoder();
    Object.freeze(this);
  }

  debugBelch2(fmt, arg) {
    const s = `${this.memory.strLoad(arg)}\n`;
    this.fs.writeNonMemory(2, this.encoder.encode(s));
  }
}
