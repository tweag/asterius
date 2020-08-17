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

export class FS {
  constructor(components) {
    this.components = components;
    this.stdout = new Device(console.log, true);
    this.stderr = new Device(console.error, true);
  }

  read(fd, buf, count) {
    throw new WebAssembly.RuntimeError(
      `Attempting to read(${fd}, ${buf}, ${count})`
    );
  }

  write(fd, buf, count) {
    buf = this.components.memory.expose(buf, count, Uint8Array);
    switch (fd) {
      case 1: {
        return this.stdout.write(buf);
      }
      case 2: {
        return this.stderr.write(buf);
      }
      default: {
        throw new WebAssembly.RuntimeError(
          `Attempting to write(${fd}, ${buf}, ${count})`
        );
      }
    }
  }

  writeNonMemory(fd, data) {
    switch (fd) {
      case 1: {
        this.stdout.write(data);
        break;
      }
      case 2: {
        this.stderr.write(data);
        break;
      }
      default: {
        throw new WebAssembly.RuntimeError(`writeNonMemory(${fd}, ${data})`);
      }
    }
  }

  history(fd) {
    switch (fd) {
      case 1: {
        return this.stdout.read();
      }
      case 2: {
        return this.stderr.read();
      }
      default: {
        throw new WebAssembly.RuntimeError(
          `Attempting to get history of ${fd}`
        );
      }
    }
  }
}
