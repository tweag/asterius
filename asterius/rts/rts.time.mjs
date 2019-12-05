import * as rtsConstants from "./rts.constants.mjs";

export class TimeCBits {
  constructor(memory) {
    this.memory = memory;
    this.epoch = Date.now();
    Object.freeze(this);
  }

  clock_getres(clk_id, res) {
    if (res) {
      this.memory.i64Store(res + rtsConstants.offset_timespec_tv_sec, 1);
      this.memory.i64Store(res + rtsConstants.offset_timespec_tv_nsec, 0);
    }
    return 0;
  }

  clock_gettime(clk_id, tp) {
    if (tp) {
      const ms = Date.now(),
        s = Math.floor(ms / 1000),
        ns = (ms - s * 1000) * 1000000;
      this.memory.i64Store(tp + rtsConstants.offset_timespec_tv_sec, s);
      this.memory.i64Store(tp + rtsConstants.offset_timespec_tv_nsec, ns);
    }
    return 0;
  }

  getMonotonicNSec() {
    return (Date.now() - this.epoch) * 1000000;
  }
}
