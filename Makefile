.PHONY: all clean distclean bytecompile test check melpa cask-install

VERSION=$(shell sed -ne 's/^;\+ *Version: *\([0-9.]\)/\1/p' lisp/pdf-tools.el)
PACKAGE=pdf-tools-$(VERSION).tar

all: $(PACKAGE)

# Create a elpa package including the server
$(PACKAGE): .cask server/epdfinfo lisp/*.el
	cask package .

# Compile the Lisp sources
bytecompile: .cask
	cask exec emacs --batch -L lisp -f batch-byte-compile lisp/*.el

# Run ERT tests
test: all
	cask exec emacs --batch -l test/run-tests.el $(PACKAGE)
check: test

# Run the autobuild script tests in docker
test-autobuild: server-test

# Run all tests
test-all: test test-autobuild

# Init cask
.cask:
	cask install

# Run the autobuild script (installing depends and compiling)
autobuild:
	cd server && ./autobuild

# Soon to be obsolete targets
melpa-build: autobuild
	cp build/epdfinfo .
install-server-deps: ;

# Create a package like melpa would.
melpa-package: $(PACKAGE)
	cp $(PACKAGE) pdf-tools-$(VERSION)-melpa.tar
	tar -u --transform='s/server/pdf-tools-$(VERSION)\/build\/server/' \
		-f pdf-tools-$(VERSION)-melpa.tar \
		$$(git ls-files server)
	tar -u --transform='s/Makefile/pdf-tools-$(VERSION)\/build\/Makefile/' \
		-f pdf-tools-$(VERSION)-melpa.tar \
		Makefile 
	tar -u --transform='s/README\.org/pdf-tools-$(VERSION)\/README/' \
		-f pdf-tools-$(VERSION)-melpa.tar \
		README.org
	tar --delete pdf-tools-0.80/epdfinfo \
		-f pdf-tools-$(VERSION)-melpa.tar

# Various clean targets
clean: server-clean
	rm -f -- $(PACKAGE)
	rm -f -- lisp/*.elc
	rm -f -- pdf-tools-readme.txt

distclean: clean server-distclean
	rm -rf -- .cask

# Server targets
server/epdfinfo: server/Makefile
	$(MAKE) -C server

server/Makefile: server/configure
	cd server && ./configure -q

server/configure: server/configure.ac
	cd server && ./autogen.sh

server-test: server/Makefile
	$(MAKE) -C server check

server-clean:
	! [ -f server/Makefile ] || $(MAKE) -C server clean

server-distclean:
	! [ -f server/Makefile ] || $(MAKE) -C server distclean
