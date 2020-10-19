export class WASI {
  constructor(components) {
    this.components = components;
    this.epoch = process.hrtime.bigint();
    Object.freeze(this);
  }

  clock_time_get(id, precision, time_p) {
    switch (id) {
      case 1: {
        this.components.memory.i64Store(
          time_p,
          process.hrtime.bigint() - this.epoch
        );
        return 0;
      }
      default: {
        throw new WebAssembly.RuntimeError(
          `clock_time_get: unsupported clock id ${id}`
        );
      }
    }
  }

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
