EMACS ?= emacs

# Handle the mess when inside Emacs.
unexport INSIDE_EMACS		#cask not like this.
ifeq ($(EMACS), t)
EMACS = emacs
endif

EMACS_VERSION = $(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')
EFLAGS = -Q --batch

# Note: If you change this, also change it in lisp/pdf-tools.el and
# server/configure.ac .
PACKAGE_VERSION = 0.70
PKGFILE_CONTENT = (define-package "pdf-tools" "$(PACKAGE_VERSION)"	\
		   "Support library for PDF documents."			\
		   (quote ((emacs "24.3") (let-alist "1.0.1")))		\
		   :keywords						\
		   (quote ("files" "multimedia")))

PACKAGE_NAME = pdf-tools-$(PACKAGE_VERSION)
PACKAGE_DIR = $(PACKAGE_NAME)

.PHONY: all clean distclean package bytecompile test check melpa cask-install

all: package

clean: 
	rm -rf -- $(PACKAGE_DIR)
	rm -f -- $(PACKAGE_NAME).tar
	rm -f -- lisp/*.elc
	! [ -f server/Makefile ] || $(MAKE) -C server clean

distclean: clean
	rm -rf .cask
	! [ -f server/Makefile ] || $(MAKE) -C server distclean

package: $(PACKAGE_NAME).tar

$(PACKAGE_NAME).tar: server/epdfinfo lisp/*.el
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

bytecompile: cask-install
	cask exec $(EMACS) $(EFLAGS) -L $(PWD)/lisp -f batch-byte-compile lisp/*.el

test: all cask-install
	cask exec $(EMACS) $(EFLAGS) -l test/run-tests.el $(PACKAGE_NAME).tar

cask-install: .cask/$(EMACS_VERSION)

.cask/$(EMACS_VERSION):
	cask install

check: bytecompile test

print-version:
	@[ -n '$(PACKAGE_VERSION)' ] && echo '$(PACKAGE_VERSION)'

install-server-deps:
	sudo apt-get install gcc g++ make automake autoconf \
		libpng-dev libz-dev libpoppler-glib-dev
	-sudo apt-get install libpoppler-private-dev
	-sudo apt-get install gtklp

melpa-build: server/epdfinfo
	-cp -p server/epdfinfo ..
	@if [ "$(shell uname -o)" = "Msys" ]; then \
		for f in $(shell ldd server/epdfinfo | awk '/mingw/ {print $$3}'); do \
			cp $$f ..; \
		done; \
	fi
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
