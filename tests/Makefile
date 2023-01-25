WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

LAUNCH=../tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
SYNTAX := camlp5o

PACKAGES := $(PACKAGES),pa_ppx_string,ounit2

ML= \
	ppx_string.ml

CMO := $(ML:.ml=.cmo)

TESTS=ppx_string.exe

all: $(TESTS)

test:: all
	mkdir -p _build && ./ppx_string.exe

ppx_string.exe: ppx_string.cmo
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(TESTS): $(EXTERNAL)

depend::
	echo "$(CMO) $(TESTS): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o \
	$(ML) \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend
