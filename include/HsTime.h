#ifndef __HSTIME_H__
#define __HSTIME_H__

#include "HsTimeConfig.h"

#if HAVE_TIME_H
#include <time.h>
#endif

long int get_current_timezone_seconds (time_t,int* pdst,char const* * pname);

#endif
