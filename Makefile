# elpa-* targets are supposed to be only invoked from a elpa
# installation.

EMACS = emacs

.PHONY: all clean elpa-check elpa-all

all: server/epdfinfo

clean: 
	rm -rf dist
	$(MAKE) -C server clean

distclean: clean
	[ -f server/Makefile ] && $(MAKE) -C server distclean

package: all
	cask package

server/epdfinfo: server/Makefile
	$(MAKE) -C server
server/Makefile: server/configure
	cd server && ./configure -q
server/configure: server/configure.ac
	cd server && ./autogen.sh

compilecheck: 
	cask exec $(EMACS) -Q -L $$PWD/lisp --batch -f batch-byte-compile lisp/*.el
	rm -f -- lisp/*.elc

test: all
	cask exec emacs -Q -batch -L $$PWD/lisp -l test/run-tests.el 

elpa-all: all
	cp -p server/epdfinfo .
	$(MAKE) elpa-check
	$(MAKE) distclean

elpa-check:
	@if [ -x epdfinfo ]; then \
		echo "Server successfully build."; \
	else \
		echo "Server not build, maybe due to missing dependencies (See README)."; \
		echo "Required: g++ make automake autoconf libpng-dev libz-dev libpoppler-glib-dev libpoppler-private-dev"; \
		false; \
	fi
