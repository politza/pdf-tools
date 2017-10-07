#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.67])
AC_INIT([epdfinfo], 0.90, [politza@fh-trier.de])
AM_INIT_AUTOMAKE([-Wall -Wno-override foreign silent-rules])
AC_CONFIG_SRCDIR([epdfinfo.h])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CXX
AC_PROG_RANLIB
AM_PROG_AR

# Checks for libraries.
HAVE_POPPLER_FIND_OPTS="no   (requires poppler-glib >= 0.22)"
HAVE_POPPLER_ANNOT_WRITE="no   (requires poppler-glib >= 0.19.4)"
HAVE_POPPLER_ANNOT_MARKUP="no (requires poppler-glib >= 0.26)"

PKG_CHECK_MODULES([png], [libpng])
PKG_CHECK_MODULES([glib], [glib-2.0])
PKG_CHECK_MODULES([poppler], [poppler])
PKG_CHECK_MODULES([poppler_glib], [poppler-glib >= 0.16.0])
PKG_CHECK_EXISTS([poppler-glib >= 0.19.4], [HAVE_POPPLER_ANNOT_WRITE=yes])
PKG_CHECK_EXISTS([poppler-glib >= 0.22], [HAVE_POPPLER_FIND_OPTS=yes])
PKG_CHECK_EXISTS([poppler-glib >= 0.26], [HAVE_POPPLER_ANNOT_MARKUP=yes])
PKG_CHECK_MODULES([zlib], [zlib])

AC_COMPILE_IFELSE(
   [AC_LANG_PROGRAM([[
     #ifndef _WIN32
       error
     #endif
   ]])], [have_w32=true], [have_w32=false])
AM_CONDITIONAL(HAVE_W32, [test "$have_w32" = true])

if test "$have_w32" = true; then
  if test "$MSYSTEM" = MINGW32 -o "$MSYSTEM" = MINGW64; then
  # glib won't work properly on msys2 without it.
    CFLAGS="-D__USE_MINGW_ANSI_STDIO=1 $CFLAGS"
  fi
fi

SAVED_CPPFLAGS=$CPPFLAGS
CPPFLAGS=$poppler_CFLAGS
AC_LANG_PUSH([C++])
# Check if we can use the -std=c++11 option.
m4_include([m4/ax_check_compile_flag.m4])
AX_CHECK_COMPILE_FLAG([-std=c++11], [HAVE_STD_CXX11=yes])

if test "$HAVE_STD_CXX11" = yes; then
  CXXFLAGS="-std=c++11 $CXXFLAGS"
fi
# Check for private poppler header.
AC_CHECK_HEADERS([Annot.h PDFDocEncoding.h], [],
    AC_MSG_ERROR([cannot find necessary  poppler-private header (see README.org)]))
AC_LANG_POP([C++])
CPPFLAGS=$SAVED_CPPFLAGS

# Setup compile time features.
if test "$HAVE_POPPLER_FIND_OPTS" = yes; then
  AC_DEFINE([HAVE_POPPLER_FIND_OPTS],1,
    [Define to 1 to enable case sensitive searching (requires poppler-glib >= 0.22).])
fi

if test "$HAVE_POPPLER_ANNOT_WRITE" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_WRITE],1,
    [Define to 1 to enable writing of annotations (requires poppler-glib >= 0.19.4).])
fi

if test "$HAVE_POPPLER_ANNOT_MARKUP" = yes; then
  AC_DEFINE([HAVE_POPPLER_ANNOT_MARKUP],1,
    [Define to 1 to enable adding of markup annotations (requires poppler-glib >= 0.26).])
fi

AC_CANONICAL_HOST
# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h strings.h err.h])

AC_MSG_CHECKING([for error.h])
SAVED_CFLAGS=$CFLAGS
CFLAGS="$poppler_CFLAGS $poppler_glib_CFLAGS"
AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
        #include <error.h>
        ],[error (0, 0, "");])],
        [AC_DEFINE([HAVE_ERROR_H],1, [Define to 1 if error.h is useable.])
        AC_MSG_RESULT([yes])],
        AC_MSG_RESULT([no]))
AC_LANG_POP([C])
CFLAGS=$SAVED_CFLAGS

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_CHECK_TYPES([ptrdiff_t])
AC_C_BIGENDIAN

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_STRTOD
AC_CHECK_FUNCS([strcspn strtol getline])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

echo
echo "Is case-sensitive searching enabled ?     ${HAVE_POPPLER_FIND_OPTS}"
echo "Is modifying text annotations enabled ?   ${HAVE_POPPLER_ANNOT_WRITE}"
echo "Is modifying markup annotations enabled ? ${HAVE_POPPLER_ANNOT_MARKUP}"
echo
