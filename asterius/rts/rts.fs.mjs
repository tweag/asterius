class Device {
  constructor(f, console_history) {
    this.flush = f;
    this.consoleHistory = console_history;
    this.history = "";
    this.buffer = "";
    this.decoder = new TextDecoder("utf-8", { fatal: true });
    Object.seal(this);
  }

  read() {
    const r = this.history;
    this.history = "";
    return r;
  }

  write(buf) {
    const str =
      typeof buf === "string"
        ? buf
        : this.decoder.decode(buf, { stream: true });
    if (this.consoleHistory) {
      this.history += str;
    }
    this.buffer += str;
    const segs = this.buffer.split("\n");
    this.buffer = segs.pop();
    for (const seg of segs) {
      this.flush(seg);
    }
    return buf.length;
  }
}

export class MemoryFileSystem {
  constructor(console_history) {
    this.files = [
      undefined,
      new Device(console.log, console_history),
      new Device(console.error, console_history)
    ];
    Object.freeze(this);
  }

  readSync(fd) {
    return this.files[fd].read();
  }

  writeSync(fd, buf) {
    return this.files[fd].write(buf);
  }
}
