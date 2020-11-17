import wasi from "wasi";

export class WASI {
  constructor(progName) {
    this.wasi = new wasi.WASI({
      args: [progName],
      env: process.env,
      preopens: { "/": "/" },
      returnOnExit: true,
    });

    this.wasiImport = {
      args_get: (...args) => {
        return this.wasi.wasiImport.args_get(...args);
      },

      args_sizes_get: (...args) => {
        return this.wasi.wasiImport.args_sizes_get(...args);
      },

      environ_get: (...args) => {
        return this.wasi.wasiImport.environ_get(...args);
      },

      environ_sizes_get: (...args) => {
        return this.wasi.wasiImport.environ_sizes_get(...args);
      },

      clock_res_get: (...args) => {
        return this.wasi.wasiImport.clock_res_get(...args);
      },

      clock_time_get: (...args) => {
        return this.wasi.wasiImport.clock_time_get(...args);
      },

      fd_advise: (...args) => {
        return this.wasi.wasiImport.fd_advise(...args);
      },

      fd_allocate: (...args) => {
        return this.wasi.wasiImport.fd_allocate(...args);
      },

      fd_close: (...args) => {
        return this.wasi.wasiImport.fd_close(...args);
      },

      fd_datasync: (...args) => {
        return this.wasi.wasiImport.fd_datasync(...args);
      },

      fd_fdstat_get: (...args) => {
        return this.wasi.wasiImport.fd_fdstat_get(...args);
      },

      fd_fdstat_set_flags: (...args) => {
        return this.wasi.wasiImport.fd_fdstat_set_flags(...args);
      },

      fd_fdstat_set_rights: (...args) => {
        return this.wasi.wasiImport.fd_fdstat_set_rights(...args);
      },

      fd_filestat_get: (...args) => {
        return this.wasi.wasiImport.fd_filestat_get(...args);
      },

      fd_filestat_set_size: (...args) => {
        return this.wasi.wasiImport.fd_filestat_set_size(...args);
      },

      fd_filestat_set_times: (...args) => {
        return this.wasi.wasiImport.fd_filestat_set_times(...args);
      },

      fd_pread: (...args) => {
        return this.wasi.wasiImport.fd_pread(...args);
      },

      fd_prestat_get: (...args) => {
        return this.wasi.wasiImport.fd_prestat_get(...args);
      },

      fd_prestat_dir_name: (...args) => {
        return this.wasi.wasiImport.fd_prestat_dir_name(...args);
      },

      fd_pwrite: (...args) => {
        return this.wasi.wasiImport.fd_pwrite(...args);
      },

      fd_read: (...args) => {
        return this.wasi.wasiImport.fd_read(...args);
      },

      fd_readdir: (...args) => {
        return this.wasi.wasiImport.fd_readdir(...args);
      },

      fd_renumber: (...args) => {
        return this.wasi.wasiImport.fd_renumber(...args);
      },

      fd_seek: (...args) => {
        return this.wasi.wasiImport.fd_seek(...args);
      },

      fd_sync: (...args) => {
        return this.wasi.wasiImport.fd_sync(...args);
      },

      fd_tell: (...args) => {
        return this.wasi.wasiImport.fd_tell(...args);
      },

      fd_write: (...args) => {
        return this.wasi.wasiImport.fd_write(...args);
      },

      path_create_directory: (...args) => {
        return this.wasi.wasiImport.path_create_directory(...args);
      },

      path_filestat_get: (...args) => {
        return this.wasi.wasiImport.path_filestat_get(...args);
      },

      path_filestat_set_times: (...args) => {
        return this.wasi.wasiImport.path_filestat_set_times(...args);
      },

      path_link: (...args) => {
        return this.wasi.wasiImport.path_link(...args);
      },

      path_open: (...args) => {
        return this.wasi.wasiImport.path_open(...args);
      },

      path_readlink: (...args) => {
        return this.wasi.wasiImport.path_readlink(...args);
      },

      path_remove_directory: (...args) => {
        return this.wasi.wasiImport.path_remove_directory(...args);
      },

      path_rename: (...args) => {
        return this.wasi.wasiImport.path_rename(...args);
      },

      path_symlink: (...args) => {
        return this.wasi.wasiImport.path_symlink(...args);
      },

      path_unlink_file: (...args) => {
        return this.wasi.wasiImport.path_unlink_file(...args);
      },

      poll_oneoff: (...args) => {
        return this.wasi.wasiImport.poll_oneoff(...args);
      },

      proc_exit: (...args) => {
        return this.wasi.wasiImport.proc_exit(...args);
      },

      proc_raise: (...args) => {
        return this.wasi.wasiImport.proc_raise(...args);
      },

      sched_yield: (...args) => {
        return this.wasi.wasiImport.sched_yield(...args);
      },

      random_get: (...args) => {
        return this.wasi.wasiImport.random_get(...args);
      },

      sock_recv: (...args) => {
        return this.wasi.wasiImport.sock_recv(...args);
      },

      sock_send: (...args) => {
        return this.wasi.wasiImport.sock_send(...args);
      },

      sock_shutdown: (...args) => {
        return this.wasi.wasiImport.sock_shutdown(...args);
      },
    };
  }

  initialize(i) {
    this.wasi.initialize(i);
  }
}
