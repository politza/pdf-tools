bin_PROGRAMS = epdfinfo
epdfinfo_CFLAGS = -Wall $(glib_CFLAGS) $(poppler_glib_CFLAGS) $(poppler_CFLAGS) \
			$(png_CFLAGS)
epdfinfo_CXXFLAGS = -Wall $(epdfinfo_CFLAGS)
epdfinfo_LDADD =  $(glib_LIBS) $(poppler_glib_LIBS) $(poppler_LIBS) \
			$(png_LIBS) libsynctex.a $(zlib_LIBS)
epdfinfo_SOURCES = epdfinfo.c epdfinfo.h poppler-hack.cc

noinst_LIBRARIES = libsynctex.a
libsynctex_a_SOURCES = synctex_parser.c synctex_parser_utils.c synctex_parser.h	\
			synctex_parser_local.h synctex_parser_utils.h
libsynctex_a_CFLAGS = -w $(zlib_CFLAGS) -DSYNCTEX_USE_LOCAL_HEADER

if HAVE_W32
epdfinfo_LDADD += -lshlwapi
endif

SYNCTEX_UPSTREAM = svn://tug.org/texlive/trunk/Build/source/texk/web2c/synctexdir
SYNCTEX_FILES = synctex_parser.c		\
		synctex_parser.h		\
		synctex_parser_readme.txt	\
		synctex_parser_utils.c		\
		synctex_parser_utils.h		\
		synctex_parser_version.txt      \
		synctex_version.h		\
		synctex_parser_advanced.h


check-local:
	@if $(MAKE) --version 2>&1 | grep -q GNU; then \
		cd test && $(MAKE) $(AM_MAKEFLAGS); \
	else \
		echo "Skipping tests in server/test (requires GNU make)"; \
	fi

synctex-pull:
	@if [ -n "$$(git status --porcelain)" ]; then \
		git status; \
		echo "Not checking-out files into a dirty work-directory"; \
		false; \
	fi
	for file in $(SYNCTEX_FILES); do \
		svn export --force $(SYNCTEX_UPSTREAM)/$$file; \
	done
