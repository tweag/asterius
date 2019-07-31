class TextFile {
  constructor() {
    this.buffer = "";
    this.decoder = new TextDecoder("utf-8");
    Object.seal(this);
  }

  read() {
    return this.buffer;
  }

  write(buf) {
    if (typeof buf === "string") {
      this.buffer += buf;
    } else {
      this.buffer += this.decoder.decode(buf, { stream: true });
    }
  }
}

export class MemoryFileSystem {
  constructor(logger) {
    this.files = [undefined, new TextFile(), new TextFile()];
    this.logger = logger;
    Object.freeze(this);
  }
  readSync(fd) {
    return this.files[fd].read();
  }
  writeSync(fd, buf) {
    this.files[fd].write(buf);
  }
}
