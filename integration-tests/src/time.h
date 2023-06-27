#include <time.h>

#ifdef _WIN32
typedef long suseconds_t;

struct timeval {
    time_t      tv_sec;
    suseconds_t tv_usec;
};

int gettimeofday(struct timeval* tv, void* unused) {
    time(&tv->tv_sec);

    // For now, we zero the number of microseconds. If we want more precision, 
    // we should expand this function to correctly determine the number of
    // microseconds since the epoch - tv_sec.
    tv->tv_usec = 0;

    return 0;
}
#else
#include <sys/time.h>
#endif
