bin_PROGRAMS = epdfinfo
epdfinfo_CFLAGS = $(glib_CFLAGS) $(poppler_CFLAGS)
epdfinfo_LDADD =  $(glib_LIBS) $(poppler_LIBS)
epdfinfo_SOURCES = epdfinfo.c epdfinfo.h

CVSCLEANFILES = Makefile.in aclocal.m4 config.h.in configure \
	depcomp install-sh missing autom4te.cache

cvs-clean: distclean
	rm -rf $(CVSCLEANFILES)
