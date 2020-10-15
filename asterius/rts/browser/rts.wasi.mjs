export class WASI {
  constructor() {}

  fd_close() {
    throw new WebAssembly.RuntimeError(`Unsupported wasi interface: fd_close`);
  }

  fd_seek() {
    throw new WebAssembly.RuntimeError(`Unsupported wasi interface: fd_seek`);
  }

  fd_write() {
    throw new WebAssembly.RuntimeError(`Unsupported wasi interface: fd_write`);
  }
}
