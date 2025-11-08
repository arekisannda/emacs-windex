EMACS ?= emacs
ELFILES := windex-utils.el windex-layout windex-purpose.el windex-windmove windex.el
ELCFILES = $(ELFILES:.el=.elc)

all: test-windex

.PHONY: clean
clean: $(ELCFILES)
	rm $(ELCFILES)

.PHONY: test-windex
test-state: $(ELCFILES)
	$(EMACS) -nw -Q -batch -L . -l ert -l test/test-windex.el \
		--eval "(ert-run-tests-batch-and-exit)"

.PHONY: compile
compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<
