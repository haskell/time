#include <time.h>

static int test_time_zone_fallback_isdst = 0;
static long int test_time_zone_fallback_timezone = 0;
static long int test_time_zone_fallback_altzone = 0;
static char test_time_zone_fallback_std_name[] = "EST";
static char test_time_zone_fallback_dst_name[] = "EDT";
static char *test_time_zone_fallback_tzname[] = {
    test_time_zone_fallback_std_name,
    test_time_zone_fallback_dst_name
};

void test_time_zone_fallback_set_state(long int timezone_seconds, long int altzone_seconds, int isdst)
{
    test_time_zone_fallback_timezone = timezone_seconds;
    test_time_zone_fallback_altzone = altzone_seconds;
    test_time_zone_fallback_isdst = isdst;
}

void test_time_zone_fallback_tzset(void)
{
}

struct tm *test_time_zone_fallback_localtime_r(const time_t *timer, struct tm *result)
{
    (void) timer;
    result->tm_isdst = test_time_zone_fallback_isdst;
    return result;
}

#define tzset test_time_zone_fallback_tzset
#define localtime_r test_time_zone_fallback_localtime_r
#define timezone test_time_zone_fallback_timezone
#define altzone test_time_zone_fallback_altzone
#define tzname test_time_zone_fallback_tzname

#define get_current_timezone_seconds test_get_current_timezone_seconds_no_altzone
#include "../../../../lib/cbits/HsTime.c"
#undef get_current_timezone_seconds

#undef HAVE_DECL_ALTZONE
#define HAVE_DECL_ALTZONE 1
#define get_current_timezone_seconds test_get_current_timezone_seconds_altzone
#include "../../../../lib/cbits/HsTime.c"
#undef get_current_timezone_seconds
