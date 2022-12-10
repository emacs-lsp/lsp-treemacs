EMACS ?= emacs
EASK ?= eask

.PHONY: all build compile clean

ci: clean build compile

all:
	$(EASK) build

build:
	$(EASK) package
	$(EASK) install

compile:
	$(EASK) compile

checkdoc:
	$(EASK) checkdoc

lint:
	$(EASK) lint

clean:
	$(EASK) clean all
