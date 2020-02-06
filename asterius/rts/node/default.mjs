/**
 * @file Implements Node.js-specific functionality.
 */
import child_process from "child_process";
import fs from "fs";
import { performance } from "perf_hooks";

class Device {
  constructor(f, console_history) {
    this.flush = f;
    this.consoleHistory = console_history;
    this.history = "";
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
    this.flush(str);
    return buf.length;
  }
}

class MemoryFileSystem {
  constructor(console_history) {
    this.files = [
      undefined,
      new Device(s => fs.writeSync(process.stdout.fd, s), console_history),
      new Device(s => fs.writeSync(process.stderr.fd, s), console_history)
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

class Posix {
  constructor(memory, rtsConstants) {
    this.memory = memory;
    this.rtsConstants = rtsConstants;
    this.dirs = new Map();
    this.lastDir = 0;
    this.errno = 0;
    Object.seal(this);
  }
  getProgArgv(argc, argv_buf) {
    const e = new TextEncoder(),
      arg_bufs = process.argv.slice(2).map((s) => e.encode(s)),
      argv_header_size = (1 + arg_bufs.length) * 8,
      argv_total_size =
        argv_header_size +
        // All strings are \0-terminated, hence the +1
        arg_bufs.reduce((acc, buf) => acc + buf.byteLength + 1, 0);
    // The total size (in bytes) of the runtime arguments cannot exceed the 1KB
    // size of the data segment we have reserved. If you wish to change this
    // number, you should also update envArgvBuf in Asterius.Builtins.Env.
    if (argv_total_size > 1024) {
      throw new WebAssembly.RuntimeError(
        `getProgArgv: exceeding buffer size for ${process.argv}`
      );
    }
    this.memory.i64Store(argc, 1 + arg_bufs.length);
    let p0 = argv_buf + 8,
      p1 = argv_buf + argv_header_size;
    arg_bufs.forEach(arg_buf => {
      this.memory.i64Store(p0, p1);
      p0 += 8;
      this.memory.expose(p1, arg_buf.byteLength, Uint8Array).set(arg_buf);
      p1 += arg_buf.byteLength;
      this.memory.i8Store(p1, 0);
      p1 += 1;
    });
  }
  get_errno() {
    return this.errno;
  }
  set_errno(e) {
    this.errno = e;
  }
  open(f, h, m) {
    try {
      return fs.openSync(this.memory.strLoad(f), h, m);
    } catch (err) {
      this.set_errno(-err.errno);
      return -1;
    }
  }
  close(f) {
    try {
      fs.closeSync(f);
      return 0;
    } catch (err) {
      this.set_errno(-err.errno);
      return -1;
    }
  }
  ftruncate(fd, len) {
    try {
      fs.ftruncateSync(fd, len);
      return 0;
    } catch (err) {
      this.set_errno(-err.errno);
      return -1;
    }
  }
  stat(f, b) {
    try {
      const r = fs.statSync(this.memory.strLoad(f));
      this.memory.i64Store(
        b + this.rtsConstants.offset_stat_mtime,
        Math.trunc(r.mtimeMs)
      );
      this.memory.i64Store(b + this.rtsConstants.offset_stat_size, r.size);
      this.memory.i64Store(b + this.rtsConstants.offset_stat_mode, r.mode);
      this.memory.i64Store(b + this.rtsConstants.offset_stat_dev, r.dev);
      this.memory.i64Store(b + this.rtsConstants.offset_stat_ino, r.ino);
      return 0;
    } catch (err) {
      this.set_errno(-err.errno);
      return -1;
    }
  }
  fstat(f, b) {
    try {
      const r = fs.fstatSync(f);
      this.memory.i64Store(
        b + this.rtsConstants.offset_stat_mtime,
        Math.trunc(r.mtimeMs)
      );
      this.memory.i64Store(b + this.rtsConstants.offset_stat_size, r.size);
      this.memory.i64Store(b + this.rtsConstants.offset_stat_mode, r.mode);
      this.memory.i64Store(b + this.rtsConstants.offset_stat_dev, r.dev);
      this.memory.i64Store(b + this.rtsConstants.offset_stat_ino, r.ino);
      return 0;
    } catch (err) {
      this.set_errno(-err.errno);
      return -1;
    }
  }
  opendir(p) {
    try {
      const dir = fs.opendirSync(this.memory.strLoad(p));
      this.dirs.set(++this.lastDir, dir);
      return this.lastDir;
    } catch (err) {
      this.set_errno(-err.errno);
      return 0;
    }
  }
  readdir(dirPtr, pDirEnt) {
    try {
      const dirent = this.dirs.get(dirPtr).readSync();
      if (dirent) {
        const l = pDirEnt & 0xffffffff;
        const { read, written } = new TextEncoder().encodeInto(
          dirent.name,
          this.memory.i8View.subarray(l, l + 4095)
        );
        if (read !== dirent.name.length)
          throw new WebAssembly.RuntimeError(
            `${dirent.name} exceeded path limit`
          );
        this.memory.i8View[l + written] = 0;
        return pDirEnt;
      } else {
        return 0;
      }
    } catch (err) {
      this.set_errno(-err.errno);
      return -1;
    }
  }
  closedir(dirPtr) {
    try {
      this.dirs.get(dirPtr).closeSync();
      this.dirs.delete(dirPtr);
      return 0;
    } catch (err) {
      this.set_errno(-err.errno);
      return -1;
    }
  }
  getenv(keyPtr, resPtr) {
    const res = process.env[this.memory.strLoad(keyPtr)];
    if (res) {
      const l = resPtr & 0xffffffff;
      const { read, written } = new TextEncoder().encodeInto(
        res,
        this.memory.i8View.subarray(l, l + 32767)
      );
      if (read !== res.length)
        throw new WebAssembly.RuntimeError(
          `${res} exceeded environment variable limit`
        );
      this.memory.i8View[l + written] = 0;
      return resPtr;
    } else {
      return 0;
    }
  }
  access(f, m) {
    try {
      fs.accessSync(this.memory.strLoad(f), m);
      return 0;
    } catch (err) {
      this.set_errno(-err.errno);
      return -1;
    }
  }
  getcwd(buf, size) {
    const cwd = process.cwd();
    const l = buf & 0xffffffff;
    const { read, written } = new TextEncoder().encodeInto(
      cwd,
      this.memory.i8View.subarray(l, l - 1 + size)
    );
    this.memory.i8View[l + written] = 0;
    if (read === cwd.length) {
      return buf;
    } else {
      this.set_errno(34);
      return 0;
    }
  }
}

class Process {
  constructor(memory) {
    this.memory = memory;
    Object.freeze(this);
  }
  runInteractiveProcess(
    args,
    workingDirectory,
    environment,
    fdStdIn, // -1
    fdStdOut, // -1
    fdStdErr, // -1
    pfdStdInput, // 9007160604360240
    pfdStdOutput, // 9007160604360264
    pfdStdError, // 9007160604360288
    childGroup,
    childUser,
    reset_int_quit_handlers,
    flags,
    failed_doing, // 9007160604360312
    err_buf
  ) {
    try {
      const cmd_args = [];
      for (let p = args; this.memory.i64Load(p); p += 8) {
        cmd_args.push(this.memory.strLoad(this.memory.i64Load(p)));
      }
      const opts = {};
      if (workingDirectory) {
        opts.cwd = this.memory.strLoad(workingDirectory);
      }
      if (environment) {
        opts.env = {};
        for (let p = environment; this.memory.i64Load(p); p += 8) {
          const pair = this.memory.strLoad(this.memory.i64Load(p)),
            i = pair.indexOf("=");
          opts.env[pair.substring(0, i)] = pair.substring(i + 1);
        }
      }
      if (childGroup) {
        opts.gid = childGroup;
      }
      if (childUser) {
        opts.uid = childUser;
      }
      child_process.spawn(cmd_args[0], cmd_args.slice(1), {});
      throw "todo";
    } catch (err) {
      throw err;
    }
  }
}

export default {
  /**
   * A custom Time interface, used in {@link TimeCBits}.
   */
  Time: {
    /**
     * Returns the current high-resolution timestamp,
     * where 0 represents the start of the node.js process.
     * @returns A [seconds, nanoseconds] Array.
     */
    getCPUTime: () => {
      const ms = performance.now(),
            s = Math.floor(ms / 1000.0),
            ns = Math.floor(ms - s * 1000) * 1000000;
      return [s, ns];
    },
    /**
     * Returns the current high-resolution timestamp,
     * where 0 represents UNIX Epoch.
     * @returns A [seconds, nanoseconds] Array.
     */
    getUnixEpochTime: () => {
      return process.hrtime();
    },
    /**
     * The resolution of the timestamp in nanoseconds
     * (high-resolution, ~~1ns).
     */
    resolution: 1
  },
  fs: MemoryFileSystem,
  posix: Posix,
  process: Process
};
