EMACS ?= emacs
EFLAGS = -Q -L $(PWD)/lisp --batch

# Note: If you change this, also change it in lisp/pdf-tools.el and
# server/configure.ac .
PACKAGE_VERSION = 0.50
PKGFILE_CONTENT = (define-package "pdf-tools" "$(PACKAGE_VERSION)"	\
		   "Support library for PDF documents."			\
		   (quote ((emacs "24.3")))				\
		   :keywords						\
		   (quote ("files" "multimedia")))

PACKAGE_NAME = pdf-tools-$(PACKAGE_VERSION)
PACKAGE_DIR = $(PACKAGE_NAME)

.PHONY: all clean distclean package bytecompile test check melpa

all: package

clean: 
	rm -rf -- $(PACKAGE_DIR)
	rm -f -- $(PACKAGE_NAME).tar
	rm -f -- lisp/*.elc
	! [ -f server/Makefile ] || $(MAKE) -C server clean

distclean: clean
	! [ -f server/Makefile ] || $(MAKE) -C server distclean

package: server/epdfinfo
	mkdir -p '$(PACKAGE_DIR)'
	cp lisp/*.el README server/epdfinfo '$(PACKAGE_DIR)'
	echo '$(PKGFILE_CONTENT)' > '$(PACKAGE_DIR)/pdf-tools-pkg.el'
	tar cf '$(PACKAGE_NAME).tar' '$(PACKAGE_DIR)'

melpa-package: 
	$(MAKE) distclean
	mkdir -p '$(PACKAGE_DIR)/build'
	cp lisp/*.el README '$(PACKAGE_DIR)'
	cp -r Makefile server '$(PACKAGE_DIR)/build'
	echo '$(PKGFILE_CONTENT)' > '$(PACKAGE_DIR)/pdf-tools-pkg.el'
	tar cf '$(PACKAGE_NAME).tar' '$(PACKAGE_DIR)'

install-package: package
	$(EMACS) $(EFLAGS) --eval \
		"(progn (package-initialize) \
			(package-install-file \
				\"$(PACKAGE_NAME).tar\"))"

server/epdfinfo: server/Makefile
	$(MAKE) -C server
server/Makefile: server/configure
	cd server && ./configure -q
server/configure: server/configure.ac
	cd server && ./autogen.sh

bytecompile: 
	$(EMACS) $(EFLAGS) -f batch-byte-compile lisp/*.el

test: all
	$(EMACS) $(EFLAGS) -l test/run-tests.el $(PACKAGE_NAME).tar

check: bytecompile test

print-version:
	@[ -n '$(PACKAGE_VERSION)' ] && echo '$(PACKAGE_VERSION)'

install-server-deps:
	sudo apt-get install gcc g++ make automake autoconf \
		libpng-dev libz-dev libpoppler-glib-dev
	-sudo apt-get install libpoppler-private-dev

melpa-build: server/epdfinfo
	-cp -p server/epdfinfo ..
	$(MAKE) distclean
	@if [ -x ../epdfinfo ]; then \
		echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~"; \
		echo "Server successfully build. "; \
		echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~"; \
	else \
		echo "Server not build, maybe due to missing dependencies (See README)."; \
		echo "Required: gcc g++ make automake autoconf libpng-dev libz-dev libpoppler-glib-dev libpoppler-private-dev"; \
		false; \
	fi
