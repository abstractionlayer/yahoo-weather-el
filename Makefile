EMACS   := emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile

EL  = yahoo-weather.el
EL += org-yahoo-weather.el

ELC = $(EL:.el=.elc)

EXTRA_DIST = README.md LICENSE

.PHONY: all compile clean test

all: compile

test:
	$(BATCH) \
		-L tests \
		-l tests/yahoo-weather-tests.el \
		-f ert-run-tests-batch-and-exit

compile: $(ELC)

clean:
	$(RM) *.elc

%.elc: %.el
	@$(COMPILE) $<
