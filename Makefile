# Inspired by https://nullprogram.com/blog/2020/01/22/

.POSIX:

EMACS = emacs
LDFLAGS = -L site-lisp $(patsubst %,-L %, $(wildcard elpa/*/))
EL = site-lisp/timeline-tools.el site-lisp/db-customize.el site-lisp/db-emms.el site-lisp/db-eshell.el site-lisp/db-hydras.el site-lisp/db-mail.el site-lisp/db-music.el site-lisp/db-org.el site-lisp/db-projects.el site-lisp/db-utils.el site-lisp/db-utils-test.el site-lisp/timeline-tools.el
TEST = $(wildcard site-lisp/*-test.el)
ELC = $(EL:.el=.elc)
TESTC = $(TEST:.el=.elc)

compile: $(ELC) $(TESTC)

timelinetools-test.elc: timeline-tools.elc

test: $(ELC) $(TESTC)
	@echo "Testing $(TESTC)"
	@$(EMACS) -Q --batch $(LDFLAGS) $(patsubst %,-l %, $(TESTC)) -f ert-run-tests-batch

.PHONY: clean distclean sandbox-start

clean:
	rm -f $(ELC) $(TESTC)

distclean: clean
	rm -rfv elpa
	git checkout elpa

sandbox-start:
	mkdir -p sandbox
	ln -s $(PWD) sandbox/.emacs.d
	HOME=$(PWD)/sandbox emacs

.SUFFIXES: .el .elc
.el.elc:
	@echo "Compiling $<"
	@$(EMACS) -Q --batch $(LDFLAGS) -f batch-byte-compile $<
