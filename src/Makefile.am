bin_PROGRAMS = epdfinfo
epdfinfo_CFLAGS = $(glib_CFLAGS) $(poppler_glib_CFLAGS) $(poppler_CFLAGS)
epdfinfo_CXXFLAGS = $(glib_CFLAGS) $(poppler_glib_CFLAGS) $(poppler_CFLAGS)
epdfinfo_LDADD =  $(glib_LIBS) $(poppler_glib_LIBS) $(poppler_LIBS) \
			libsynctex.a $(zlib_LIBS)
epdfinfo_SOURCES = epdfinfo.c epdfinfo.h poppler-hack.cc

noinst_LIBRARIES = libsynctex.a
libsynctex_a_SOURCES = synctex_parser.c synctex_parser_utils.c synctex_parser.h	\
			synctex_parser_local.h synctex_parser_utils.h
libsynctex_a_CFLAGS = -w
