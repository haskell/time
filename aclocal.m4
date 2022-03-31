# FP_DECL_ALTZONE
# ---------------
# Defines HAVE_DECL_ALTZONE to 1 if declared, 0 otherwise.
#
# Used by base package.
AC_DEFUN([FP_DECL_ALTZONE],
[
  AC_CHECK_HEADERS_ONCE([sys/time.h])

  AC_CHECK_HEADERS([sys/time.h])
  AC_CHECK_DECLS([altzone], [], [],[
  #if HAVE_SYS_TIME_H
  #include <sys/time.h>
  #endif
  #include <time.h>
  ])
])# FP_DECL_ALTZONE
