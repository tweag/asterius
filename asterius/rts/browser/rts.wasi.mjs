export class WASI {
  constructor() {}

  fd_close() {
    throw new WebAssembly.RuntimeError(`Unsupported wasi interface: fd_close`);
  }

  fd_prestat_dir_name() {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi interface: fd_prestat_dir_name`
    );
  }

  fd_prestat_get() {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi interface: fd_prestat_get`
    );
  }

  fd_seek() {
    throw new WebAssembly.RuntimeError(`Unsupported wasi interface: fd_seek`);
  }

  fd_write() {
    throw new WebAssembly.RuntimeError(`Unsupported wasi interface: fd_write`);
  }

  proc_exit() {
    throw new WebAssembly.RuntimeError(`Unsupported wasi interface: proc_exit`);
  }
}
