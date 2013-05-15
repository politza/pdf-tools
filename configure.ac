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
AM_PATH_LISPDIR                 dnl Sets EMACS

# Checks for libraries.
HAVE_POPPLER_FIND_OPTS="no   (requires poppler-glib >= 0.22)"
HAVE_POPPLER_ANNOT_WRITE="no   (requires poppler-glib >= 0.19.4)"

PKG_CHECK_MODULES([glib], [glib-2.0])
PKG_CHECK_MODULES([poppler], [poppler])
PKG_CHECK_MODULES([poppler_glib], [poppler-glib >= 0.16.0])
PKG_CHECK_EXISTS([poppler-glib >= 0.22], [HAVE_POPPLER_FIND_OPTS=yes])
PKG_CHECK_EXISTS([poppler-glib >= 0.19.4], [HAVE_POPPLER_ANNOT_WRITE=yes])

if test "$HAVE_POPPLER_FIND_OPTS" = yes; then
  AC_DEFINE([HAVE_POPPLER_FIND_OPTS],1,
    [Define to 1 to enable case sensitive searching (requires poppler-glib >= 0.22).])
fi

if test "$HAVE_POPPLER_ANNOT_WRITE" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_WRITE],1,
    [Define to 1 to enable writing of annotations (requires poppler-glib >= 0.19.4).])
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

echo
echo "Is case-sensitive searching enabled ? ${HAVE_POPPLER_FIND_OPTS}"
echo "Is writing annotations enabled ?      ${HAVE_POPPLER_ANNOT_WRITE}"
echo
