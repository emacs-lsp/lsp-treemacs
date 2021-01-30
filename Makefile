.PHONY: all build unix-compile windows-compile clean

EMACS ?= emacs
CASK ?= cask

LSP-TREEMACS-GENERAL := lsp-treemacs.el lsp-treemacs-themes.el

all:
	$(CASK) build

build:
	$(CASK) install

# NOTE: treemacs also sets treemacs-no-load-time-warnings to t in its Makefile, so I guess it's OK?
unix-compile:
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(LSP-TREEMACS-GENERAL)

windows-compile:
	@$(CASK) $(EMACS) -Q --batch \
		-l test/windows-bootstrap.el \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(LSP-TREEMACS-GENERAL)

unix-ci: clean build unix-compile

windows-ci: CASK=
windows-ci: clean windows-compile

clean:
	rm -rf .cask *.elc
