dnl A skeletal configure script to do enough basic testing of
dnl headers, types, etc. to compile nlopt.

AC_INIT(nlopt, 2.4.2, sam@nrel.gov)
AC_CONFIG_MACRO_DIR([m4])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([unistd.h getopt.h stdint.h])
AC_C_CONST
AC_C_INLINE
AX_C_THREADLOCAL

AC_CHECK_TYPES(uint32_t, [], [], [$ac_includes_default
#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif])

dnl Checks for libraries and functions
AC_CHECK_LIB(m, sin)
AC_CHECK_FUNCS([BSDgettimeofday gettimeofday time qsort_r getpid])

AC_MSG_CHECKING([for gettid syscall])
AC_TRY_LINK([#include <unistd.h>
#include <sys/syscall.h>
], [syscall(SYS_gettid);], [ok=yes], [ok=no])
if test "$ok" = "yes"; then
	AC_DEFINE(HAVE_GETTID_SYSCALL,1,[Define if syscall(SYS_gettid) available.])
fi
AC_MSG_RESULT(${ok})

AC_MSG_CHECKING([for isnan])
AC_TRY_LINK([#include <math.h>
], [if (!isnan(3.14159)) isnan(2.7183);], ok=yes, ok=no)
if test "$ok" = "yes"; then
	AC_DEFINE(HAVE_ISNAN,1,[Define if the isnan() function/macro is available.])
fi
AC_MSG_RESULT(${ok})

AC_MSG_CHECKING([for isinf])
AC_TRY_LINK([#include <math.h>
], [if (!isinf(3.14159)) isinf(2.7183);], ok=yes, ok=no)
if test "$ok" = "yes"; then
	AC_DEFINE(HAVE_ISINF,1,[Define if the isinf() function/macro is available.])
fi
AC_MSG_RESULT(${ok})

AC_CONFIG_HEADERS([config.h:config.h.in])

AC_MSG_CHECKING([for copysign])
AC_TRY_LINK([#include <math.h>
], [double x = copysign(3.14159, -2.7183);], ok=yes, ok=no)
if test "$ok" = "yes"; then
	AC_DEFINE(HAVE_COPYSIGN,1,[Define if the copysign function/macro is available.])
fi
AC_MSG_RESULT(${ok})

vers=`echo ${PACKAGE_VERSION} | cut -d. -f1`
AC_DEFINE_UNQUOTED(MAJOR_VERSION, $vers, [Major version number.])
vers=`echo ${PACKAGE_VERSION} | cut -d. -f2`
AC_DEFINE_UNQUOTED(MINOR_VERSION, $vers, [Minor version number.])
vers=`echo ${PACKAGE_VERSION} | cut -d. -f3`
AC_DEFINE_UNQUOTED(BUGFIX_VERSION, $vers, [Bugfix version number.])

AC_OUTPUT	  
