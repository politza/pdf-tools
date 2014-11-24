SUBDIRS = src tests

LISP_FILES = $(top_srcdir)/lisp/pdf-history.el		\
		$(top_srcdir)/lisp/pdf-info.el		\
		$(top_srcdir)/lisp/pdf-isearch.el	\
		$(top_srcdir)/lisp/pdf-links.el		\
		$(top_srcdir)/lisp/pdf-misc.el		\
		$(top_srcdir)/lisp/pdf-occur.el		\
		$(top_srcdir)/lisp/pdf-outline.el	\
		$(top_srcdir)/lisp/pdf-tools.el		\
		$(top_srcdir)/lisp/pdf-util.el		\
		$(top_srcdir)/lisp/pdf-annot.el		\
		$(top_srcdir)/lisp/pdf-sync.el		\
		$(top_srcdir)/lisp/pdf-view.el		\
		$(top_srcdir)/lisp/pdf-cache.el		\
		$(top_srcdir)/lisp/tablist.el		\
		$(top_srcdir)/lisp/tablist-filter.el

AUX_FILES = $(top_srcdir)/README.org			\
		$(top_srcdir)/images/notavailable.xpm	\
		$(top_srcdir)/images/paperclip.xpm	\
		$(top_srcdir)/images/speech411.xpm

# Emacs Lisp Package
ELP_FILES = $(LISP_FILES) $(AUX_FILES) $(top_builddir)/src/epdfinfo
ELP_NAME = $(PACKAGE_NAME)-$(PACKAGE_VERSION)
ELP_TAR_FILE = $(ELP_NAME).tar
ELP_TMP_DIR = elp-tmp-dir

EXTRA_DIST = $(LISP_FILES) $(AUX_FILES)
CLEANFILES = $(ELP_TAR_FILE)

all-local: $(ELP_TAR_FILE)

$(ELP_TAR_FILE): $(LISP_FILES)
	mkdir -p $(ELP_TMP_DIR)/$(ELP_NAME)
	echo "(define-package \"$(PACKAGE_NAME)\" \"$(PACKAGE_VERSION)\" \
	 	\"Support library for PDF documents.\")"  \
		> $(ELP_TMP_DIR)/$(ELP_NAME)/$(PACKAGE_NAME)-pkg.el
	cp -r $(ELP_FILES) -t $(ELP_TMP_DIR)/$(ELP_NAME)/
	cd $(ELP_TMP_DIR) && \
		mkdir $(ELP_NAME)/images && \
		mv $(ELP_NAME)/*.xpm $(ELP_NAME)/images && \
		tar cf ../$(ELP_TAR_FILE) $(ELP_NAME) 
	rm -rf -- $(ELP_TMP_DIR)

install-package: all $(ELP_TAR_FILE)
	if ! which -- "$(EMACS)"; then \
		echo "No emacs executable found, you have to set EMACS"; \
		false; \
	fi
	$(EMACS) -Q --batch --eval "(package-install-file \
	 	\"$$PWD/$(ELP_TAR_FILE)\")"


