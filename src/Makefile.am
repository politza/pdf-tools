bin_PROGRAMS = epdfinfo
AM_CFLAGS = $(glib_CFLAGS) $(poppler_glib_CFLAGS) $(poppler_CFLAGS) -w
AM_CXXFLAGS = $(glib_CFLAGS) $(poppler_glib_CFLAGS) $(poppler_CFLAGS) -w
epdfinfo_LDADD =  $(glib_LIBS) $(poppler_glib_LIBS) $(poppler_LIBS) 
epdfinfo_SOURCES = epdfinfo.c epdfinfo.h poppler-hack.cc

CVSCLEANFILES = Makefile.in aclocal.m4 config.h.in configure \
	depcomp install-sh missing autom4te.cache

cvs-clean: distclean
	rm -rf $(CVSCLEANFILES)

