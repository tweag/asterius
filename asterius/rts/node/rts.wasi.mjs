import wasi from "wasi";

export class WASI {
  constructor(progName) {
    this.wasi = new wasi.WASI({
      args: [progName],
      env: process.env,
      preopens: { "/": "/" },
      returnOnExit: true,
    });
    Object.freeze(this);
  }

  get wasiImport() {
    return this.wasi.wasiImport;
  }

  initialize(i) {
    this.wasi.initialize(i);
  }
}
