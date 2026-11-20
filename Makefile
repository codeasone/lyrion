EMACS ?= emacs
# Headless: with no display, nothing a test starts can open a window. Input
# is closed too, so a stray question fails at once instead of waiting.
BATCH = env -u DISPLAY -u WAYLAND_DISPLAY -u XAUTHORITY $(EMACS) -Q --batch -L .
NOINPUT = < /dev/null

.PHONY: all test lint coverage

all: lint test

test:
	$(BATCH) -L test -l test/lyrion-test.el -f ert-run-tests-batch-and-exit $(NOINPUT)

# The share of lyrion.el's lines the tests must run, as a percentage.
COVERAGE_MIN ?= 90

coverage:
	rm -f lyrion.elc coverage/lcov.info
	$(BATCH) -L test -l test/coverage.el -l test/lyrion-test.el -f ert-run-tests-batch-and-exit $(NOINPUT)
	awk -F'[:,]' -v min=$(COVERAGE_MIN) '/^DA:/ { n++; if ($$3 > 0) hit++ } END { p = 100 * hit / n; printf "Coverage: %.1f%% of %d lines (minimum %d%%)\n", p, n, min; exit p < min }' coverage/lcov.info

lint:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile lyrion.el test/lyrion-test.el
	$(BATCH) --eval "(setq sentence-end-double-space nil)" --eval "(checkdoc-file \"lyrion.el\")"
	rm -f *.elc test/*.elc
