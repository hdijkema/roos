MAKE            ?= make
RACKET          := racket
RACO            := raco
SCRIBBLE        := $(RACO) scribble

DO-DOCS         := --no-docs
INSTALL-FLAGS   := --auto --skip-installed $(DO-DOCS)
REMOVE-FLAGS    := --force --no-trash $(DO-DOCS)
DEPS-FLAGS      := --check-pkg-deps --unused-pkg-deps
SETUP-FLAGS     := --tidy --avoid-main $(DEPS-FLAGS)
TEST-FLAGS      := --heartbeat --no-run-if-absent --submodule test --table

PWD             ?= $(shell pwd)

all: clean compile

clean:
	find $(PWD) -type d -name "compiled" -exec rm -dr {} +

compile:
	$(RACKET) -e "(require compiler/compiler setup/getinfo) (compile-directory-zos (path->complete-path \"$(PWD)\") (get-info/full \"$(PWD)/info.rkt\") #:skip-doc-sources? #t #:verbose #f)"

install:
	$(RACO) pkg install $(INSTALL-FLAGS) --name dirname

setup:
	$(RACO) setup $(SETUP-FLAGS) --pkgs dirname

test:
	$(RACO) test $(TEST-FLAGS) --package dirname

remove:
	$(RACO) pkg remove $(REMOVE-FLAGS) dirname
