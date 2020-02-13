import * as rtsConstants from "./rts.constants.mjs";

export class TimeCBits {
  constructor(memory, targetSpecificModule) {
    this.memory = memory;
    // Obtain Time API from the passed target-specific module
    this.resolution = targetSpecificModule.Time.resolution; // format: ns
    this.getCPUTime = targetSpecificModule.Time.getCPUTime; // format: [s,ns]
    this.getUnixEpochTime = targetSpecificModule.Time.getUnixEpochTime; // format: [s,ns]
    Object.freeze(this);
  }

  /**
   * Returns a (monotonic) nanoseconds timestamp.
   */
  getMonotonicNSec() {
    const time = this.getCPUTime();
    return time[0] * 1000000000 + time[1]; 
  }

  /**
   * Stores at a memory address the resolution of a given clock.
   * @param clk_id the type of requested clock
   *   ({@link rtsConstants.clock_monotonic} or {@link rtsConstants.clock_realtime})
   * @param addr the memory address 
   */
  clock_getres(clk_id, addr) {
    if (addr) {
      let sec = 0, nsec = this.resolution;
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
   * @param clk_id the type of requested clock
   *   ({@link rtsConstants.clock_monotonic} or {@link rtsConstants.clock_realtime})
   * @param addr the memory address 
   */
  clock_gettime(clk_id, addr) {
    if (addr) {
      // fallback by default on the realtime timer
      const time = clk_id == rtsConstants.clock_monotonic ? this.getCPUTime() : this.getUnixEpochTime();
      this.memory.i64Store(addr + rtsConstants.offset_timespec_tv_sec, time[0]);
      this.memory.i64Store(addr + rtsConstants.offset_timespec_tv_nsec, time[1]);
    }
    return 0;
  }
}
