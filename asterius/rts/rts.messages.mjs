export class Messages {
  constructor(memory, fs) {
    this.memory = memory;
    this.fs = fs;
    Object.freeze(this);
  }

  debugBelch2(fmt, arg) {
    const s = `${this.memory.strLoad(arg)}\n`;
    this.fs.writeSync(2, s);
  }
}
