import { modulify } from "./rts.modulify.mjs";

export class WASI {
  constructor() {}

  get wasiImport() {
    return modulify(this);
  }

  initialize() {}

  args_get() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: args_get");
  }

  args_sizes_get() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: args_sizes_get"
    );
  }

  environ_get() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: environ_get");
  }

  environ_sizes_get() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: environ_sizes_get"
    );
  }

  clock_res_get() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: clock_res_get"
    );
  }

  clock_time_get() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: clock_time_get"
    );
  }

  fd_advise() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_advise");
  }

  fd_allocate() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_allocate");
  }

  fd_close() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_close");
  }

  fd_datasync() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_datasync");
  }

  fd_fdstat_get() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: fd_fdstat_get"
    );
  }

  fd_fdstat_set_flags() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: fd_fdstat_set_flags"
    );
  }

  fd_fdstat_set_rights() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: fd_fdstat_set_rights"
    );
  }

  fd_filestat_get() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: fd_filestat_get"
    );
  }

  fd_filestat_set_size() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: fd_filestat_set_size"
    );
  }

  fd_filestat_set_times() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: fd_filestat_set_times"
    );
  }

  fd_pread() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_pread");
  }

  fd_prestat_get() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: fd_prestat_get"
    );
  }

  fd_prestat_dir_name() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: fd_prestat_dir_name"
    );
  }

  fd_pwrite() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_pwrite");
  }

  fd_read() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_read");
  }

  fd_readdir() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_readdir");
  }

  fd_renumber() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_renumber");
  }

  fd_seek() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_seek");
  }

  fd_sync() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_sync");
  }

  fd_tell() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_tell");
  }

  fd_write() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: fd_write");
  }

  path_create_directory() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: path_create_directory"
    );
  }

  path_filestat_get() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: path_filestat_get"
    );
  }

  path_filestat_set_times() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: path_filestat_set_times"
    );
  }

  path_link() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: path_link");
  }

  path_open() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: path_open");
  }

  path_readlink() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: path_readlink"
    );
  }

  path_remove_directory() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: path_remove_directory"
    );
  }

  path_rename() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: path_rename");
  }

  path_symlink() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: path_symlink"
    );
  }

  path_unlink_file() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: path_unlink_file"
    );
  }

  poll_oneoff() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: poll_oneoff");
  }

  proc_exit() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: proc_exit");
  }

  proc_raise() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: proc_raise");
  }

  sched_yield() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: sched_yield");
  }

  random_get() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: random_get");
  }

  sock_recv() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: sock_recv");
  }

  sock_send() {
    throw new WebAssembly.RuntimeError("Unsupported wasi syscall: sock_send");
  }

  sock_shutdown() {
    throw new WebAssembly.RuntimeError(
      "Unsupported wasi syscall: sock_shutdown"
    );
  }
}
