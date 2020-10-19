#include <time.h>
#include "HsFFI.h"

int ghczuwrapperZC0ZCbaseZCSystemziCPUTimeziPosixziClockGetTimeZCclockzugetres(clockid_t clk_id, struct timespec *res) {
  return clock_getres(clk_id, res);
}

int ghczuwrapperZC0ZCbaseZCSystemziCPUTimeziPosixziClockGetTimeZCclockzugettime(clockid_t clk_id, struct timespec *tp) {
  return clock_gettime(clk_id, tp);
}

StgWord64 getMonotonicNSec(void)
{
    struct timespec ts;
    int res;

    res = clock_gettime(CLOCK_MONOTONIC, &ts);
    return (StgWord64)ts.tv_sec * 1000000000 +
           (StgWord64)ts.tv_nsec;
}
