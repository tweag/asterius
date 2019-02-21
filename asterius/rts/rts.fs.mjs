export class MemoryFileSystem {
  constructor(logger) {
    this.root = new Map([ [ "/dev/stdout", "" ], [ "/dev/stderr", "" ] ]);
    this.fds = [, "/dev/stdout", "/dev/stderr" ];
    this.logger = logger;
    Object.seal(this);
  }
  stdout() { return 1; }
  stderr() { return 2; }
  openSync(path, flags) {
    if (!this.root.has(path)) this.root.set(path, "");
    return this.fds.push(path) - 1;
  }
  closeSync(fd) { delete this.fds[fd]; }
  writeSync(fd, string) {
    const p = this.fds[fd];
    this.root.set(p, this.root.get(p) + string);
    this.logger.logEvent([ fd, string ]);
  }
}
