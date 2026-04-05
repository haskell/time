#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef CLOCK_TAI
#include <unistd.h>
#endif

int main(void)
{
#ifndef CLOCK_TAI
    puts("CLOCK_TAI is not defined on this platform; skipping TAI wait.");
    return 0;
#else
    long const timeout_seconds = 300;
    long const poll_milliseconds = 1000;
    time_t start;

    start = time(NULL);
    for (;;) {
        struct timespec ts;
        if (clock_gettime(CLOCK_TAI, &ts) == 0) {
            printf("CLOCK_TAI is usable: %lld.%09ld\n", (long long) ts.tv_sec, ts.tv_nsec);
            return 0;
        }

        if (errno != EINVAL) {
            fprintf(stderr, "clock_gettime(CLOCK_TAI) failed: %s\n", strerror(errno));
            return 1;
        }

        if (difftime(time(NULL), start) >= timeout_seconds) {
            fprintf(stderr, "CLOCK_TAI did not become usable within %ld seconds\n", timeout_seconds);
            return 1;
        }

        usleep((useconds_t) poll_milliseconds * 1000);
    }
#endif
}
