bin_PROGRAMS = epdfinfo
AM_CFLAGS = $(glib_CFLAGS) $(poppler_glib_CFLAGS) $(poppler_CFLAGS)
AM_CXXFLAGS = $(glib_CFLAGS) $(poppler_glib_CFLAGS) $(poppler_CFLAGS)
epdfinfo_LDADD =  $(glib_LIBS) $(poppler_glib_LIBS) $(poppler_LIBS) $(zlib_LIBS)
epdfinfo_SOURCES = epdfinfo.c epdfinfo.h poppler-hack.cc synctex_parser.c	\
	synctex_parser_utils.c synctex_parser.h synctex_parser_local.h		\
	synctex_parser_utils.h
