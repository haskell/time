#ifndef TEST_UNIX_LOCALTIME_HSTIME_H
#define TEST_UNIX_LOCALTIME_HSTIME_H

#include <time.h>

#define HAVE_LOCALTIME_R 1
#define HAVE_TZSET 1
#define HAVE_TM_ZONE 0
#define HAVE_TZNAME 1
#define HAVE_DECL_ALTZONE 0

long int get_current_timezone_seconds(time_t, int *pdst, char const **pname);

#endif
