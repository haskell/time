#include "timestuff.h"

long int get_current_timezone_seconds (time_t t,int* dst)
{
	struct tm tmd;
	struct tm* ptm = localtime_r(&t,&tmd);
	if (ptm)
	{
		*dst = ptm -> tm_isdst;
		return ptm -> tm_gmtoff;
	}
	else return 0x80000000;
}
