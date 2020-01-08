import * as rtsConstants from "./rts.constants.mjs";

export class TimeCBits {
  constructor(memory, targetSpecificModule) {
    this.memory = memory;
    // Obtain Time API from the passed target-specific module
    this.resolution = targetSpecificModule.Time.resolution; // ns
    this.now = targetSpecificModule.Time.now; // output: ms
    this.time = targetSpecificModule.Time.time; // output: [s,ns]
    Object.freeze(this);
  }

  /**
   * Returns a (monotonic) nanoseconds timestamp.
   */
  getMonotonicNSec() {
    // convert this.now() from ms to ns
    return Math.floor(this.now() * 1000000);
  }

  /**
   * Stores at a memory address the resolution of a given clock.
   * @param clk_id the id of the clock
   * @param addr the memory address 
   */
  clock_getres(clk_id, addr) {
    if (addr) {
      var sec = 0, nsec = this.resolution;
      if (nsec > 1000000000) { // more than 1s
        sec = Math.floor(this.resolution / 1000000000);
        nsec = 0;
      }
      this.memory.i64Store(addr + rtsConstants.offset_timespec_tv_sec, sec);
      this.memory.i64Store(addr + rtsConstants.offset_timespec_tv_nsec, nsec);
    }
    return 0;
  }

  /**
   * Stores at a memory address the time of a given clock.
   * @param clk_id the id of the clock
   * @param addr the memory address 
   */
  clock_gettime(clk_id, addr) {
    if (addr) {
      const time = this.time();
      this.memory.i64Store(addr + rtsConstants.offset_timespec_tv_sec, time[0]);
      this.memory.i64Store(addr + rtsConstants.offset_timespec_tv_nsec, time[1]);
    }
    return 0;
  }
}
