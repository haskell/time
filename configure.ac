AC_INIT([Haskell time package], [0.3.1], [ashley@semantic.org], [time])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsTime.h])

AC_CONFIG_HEADERS([include/HsTimeConfig.h])

AC_CHECK_HEADERS([time.h])
AC_CHECK_FUNCS([gmtime_r localtime_r])

AC_STRUCT_TM
AC_STRUCT_TIMEZONE

FP_DECL_ALTZONE

AC_OUTPUT
