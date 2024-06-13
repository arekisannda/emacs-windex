EMACS ?= emacs
ELFILES := windex-state.el windex.el
ELCFILES = $(ELFILES:.el=.elc)

all: test-state

.PHONY: clean
clean: $(ELCFILES)
	rm $(ELCFILES)

.PHONY: test-state
test-state: $(ELCFILES)
	$(EMACS) -nw -Q -batch -L . -l ert -l test/test-windex-state.el \
		--eval "(ert-run-tests-batch-and-exit)"

.PHONY: compile
compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<
