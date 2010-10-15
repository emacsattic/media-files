#ELS=$(wildcard *.el)
SRCS=media-files episode-names
ELS=$(foreach x,$(SRCS),$x.el)
ELCS=$(ELS:.el=.elc)

EMACS=emacs
BATCH=$(EMACS) -batch -q -no-site-file -eval \
  '(setq load-path (cons (expand-file-name ".") load-path))'

all: $(ELCS)

%.elc: %.el
	@echo "[C] $<"
	@$(BATCH) -f batch-byte-compile "$<"

clean:
	rm -fr $(ELCS)
	rm -fr $(fLOADDEFS)
