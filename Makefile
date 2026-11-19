EMACS ?= emacs
# Headless: with no display, nothing a test starts can open a window. Input
# is closed too, so a stray question fails at once instead of waiting.
BATCH = env -u DISPLAY -u WAYLAND_DISPLAY -u XAUTHORITY $(EMACS) -Q --batch -L .
NOINPUT = < /dev/null

.PHONY: all test lint

all: lint test

test:
	$(BATCH) -L test -l test/lyrion-test.el -f ert-run-tests-batch-and-exit $(NOINPUT)

lint:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile lyrion.el test/lyrion-test.el
	$(BATCH) --eval "(setq sentence-end-double-space nil)" --eval "(checkdoc-file \"lyrion.el\")"
	rm -f *.elc test/*.elc
