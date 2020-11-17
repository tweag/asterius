import { modulify } from "./rts.modulify.mjs";

export class WASI {
  constructor() {}

  get wasiImport() {
    return modulify(this);
  }

  initialize() {}

  args_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: args_get(${args})`
    );
  }

  args_sizes_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: args_sizes_get(${args})`
    );
  }

  environ_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: environ_get(${args})`
    );
  }

  environ_sizes_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: environ_sizes_get(${args})`
    );
  }

  clock_res_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: clock_res_get(${args})`
    );
  }

  clock_time_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: clock_time_get(${args})`
    );
  }

  fd_advise(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_advise(${args})`
    );
  }

  fd_allocate(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_allocate(${args})`
    );
  }

  fd_close(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_close(${args})`
    );
  }

  fd_datasync(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_datasync(${args})`
    );
  }

  fd_fdstat_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_fdstat_get(${args})`
    );
  }

  fd_fdstat_set_flags(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_fdstat_set_flags(${args})`
    );
  }

  fd_fdstat_set_rights(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_fdstat_set_rights(${args})`
    );
  }

  fd_filestat_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_filestat_get(${args})`
    );
  }

  fd_filestat_set_size(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_filestat_set_size(${args})`
    );
  }

  fd_filestat_set_times(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_filestat_set_times(${args})`
    );
  }

  fd_pread(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_pread(${args})`
    );
  }

  fd_prestat_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_prestat_get(${args})`
    );
  }

  fd_prestat_dir_name(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_prestat_dir_name(${args})`
    );
  }

  fd_pwrite(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_pwrite(${args})`
    );
  }

  fd_read(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_read(${args})`
    );
  }

  fd_readdir(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_readdir(${args})`
    );
  }

  fd_renumber(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_renumber(${args})`
    );
  }

  fd_seek(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_seek(${args})`
    );
  }

  fd_sync(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_sync(${args})`
    );
  }

  fd_tell(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_tell(${args})`
    );
  }

  fd_write(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: fd_write(${args})`
    );
  }

  path_create_directory(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_create_directory(${args})`
    );
  }

  path_filestat_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_filestat_get(${args})`
    );
  }

  path_filestat_set_times(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_filestat_set_times(${args})`
    );
  }

  path_link(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_link(${args})`
    );
  }

  path_open(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_open(${args})`
    );
  }

  path_readlink(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_readlink(${args})`
    );
  }

  path_remove_directory(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_remove_directory(${args})`
    );
  }

  path_rename(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_rename(${args})`
    );
  }

  path_symlink(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_symlink(${args})`
    );
  }

  path_unlink_file(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: path_unlink_file(${args})`
    );
  }

  poll_oneoff(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: poll_oneoff(${args})`
    );
  }

  proc_exit(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: proc_exit(${args})`
    );
  }

  proc_raise(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: proc_raise(${args})`
    );
  }

  sched_yield(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: sched_yield(${args})`
    );
  }

  random_get(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: random_get(${args})`
    );
  }

  sock_recv(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: sock_recv(${args})`
    );
  }

  sock_send(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: sock_send(${args})`
    );
  }

  sock_shutdown(...args) {
    throw new WebAssembly.RuntimeError(
      `Unsupported wasi syscall: sock_shutdown(${args})`
    );
  }
}
