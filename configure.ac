#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.67])
AC_INIT([pdf-tools], [0.2alpha1], [politza@fh-trier.de])
AM_INIT_AUTOMAKE([-Wall -Wno-override foreign])
AC_CONFIG_SRCDIR([src/epdfinfo.h])
AC_CONFIG_HEADERS([src/config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AM_PATH_LISPDIR

# Checks for libraries.
PKG_CHECK_MODULES([glib], [glib-2.0])
PKG_CHECK_MODULES([poppler], [poppler])
PKG_CHECK_MODULES([poppler_glib], [poppler-glib])

poppler_version=${POPPLER_GLIB_VERSION}

if test -z $poppler_version; then
 poppler_version='< 0.12.0'
 for v in 0.22.0 0.16.0 0.12.0; do
     AC_MSG_CHECKING([for poppler-glib >= $v])
     PKG_CHECK_EXISTS([poppler-glib >= $v], [], [v=])
     if test -n "$v"; then
       AC_MSG_RESULT([yes])
       poppler_version=$v
       break
     fi
     AC_MSG_RESULT([no])
 done
fi

poppler_features=0
case $poppler_version in
  0.12.0) poppler_features=1;;
  0.16.0) poppler_features=2;;
  0.22.0) poppler_features=3;;
esac

if test $poppler_features -ne 0; then
  AC_DEFINE([HAVE_POPPLER_GET_TITLE],1,[Define to 1 to enable extracting PDF title (requires poppler-glib >= 0.12).])
  AC_DEFINE([HAVE_POPPLER_INDEX_ITER],1,[Define to 1 to enable extracting outlines (requires poppler-glib >= 0.12).])
fi
if test $poppler_features -ge 2; then
  AC_DEFINE([HAVE_POPPLER_GET_METADATA],1,
      [Define to 1 to enable extracting metadata (requires poppler-glib >= 0.16).])
  AC_DEFINE([HAVE_POPPLER_GET_TEXT],1,
      [Define to 1 to enable extracting text (requires poppler-glib >= 0.16).])
fi
if test $poppler_features -ge 3; then
  AC_DEFINE([HAVE_POPPLER_FIND_OPTS],1,
    [Define to 1 to enable case sensitive searching (requires poppler-glib >= 0.22).])
fi

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h strings.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_STRTOD
AC_CHECK_FUNCS([strcspn strtol])

AC_CONFIG_FILES([src/Makefile Makefile])
AC_OUTPUT


if test $poppler_features -lt 3; then
   AC_MSG_WARN([poppler-glib version found is < 0.22 ($poppler_version).  Therefore some features were disabled, see src/config.h for details.])
fi
