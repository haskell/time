#include "HsTime.h"
#include <stdio.h>

long int get_current_timezone_seconds (time_t t,int* pdst,char const* * pname)
{
	struct tm* ptm;
	long gmtoff;
	int dst;
	const char *name;

#if HAVE_LOCALTIME_R
	struct tm tmd;
	ptm = localtime_r(&t,&tmd);
#else
	ptm = localtime(&t);
#endif
	// We don't have a better API to use on Windows, the logic to
	// decide whether a given data/time falls within DST is
	// implemented as part of localtime() in the CRT.  This is_dst
	// flag is all we need here.

	if (ptm)
	{
	        dst = ptm -> tm_isdst;
#if HAVE_TM_ZONE
		name = ptm -> tm_zone;
		gmtoff = ptm -> tm_gmtoff;
#else

# if mingw32_HOST_OS
		name = dst ? _tzname[1] : _tzname[0];
		printf("dst: %d, tzname0: %s, tzname1: %s\n", dst, _tzname[0], _tzname[1]);
# elif HAVE_TZNAME
		name = *tzname;
# else
#  error "Don't know how to get at timezone name on your OS"
# endif

# if mingw32_HOST_OS
		gmtoff = dst ? _timezone - 3600 : _timezone;
# elif HAVE_DECL_ALTZONE
		gmtoff = dst ? altzone : timezone;
# else
		gmtoff = dst ? timezone - 3600 : timezone;
# endif

#endif // HAVE_TM_ZONE
	        *pdst  = dst;
		*pname = name;
		return gmtoff;

	}
	else return 0x80000000;
}
