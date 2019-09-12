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
      return buf.length;
    }
  }
}

export class MemoryFileSystem {
  constructor() {
    this.files = [undefined, new TextFile(), new TextFile()];
    Object.freeze(this);
  }
  readSync(fd) {
    return this.files[fd].read();
  }
  writeSync(fd, buf) {
    return this.files[fd].write(buf);
  }
}
