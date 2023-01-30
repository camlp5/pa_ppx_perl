WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

LAUNCH=../tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
SYNTAX := camlp5o

PACKAGES := $(PACKAGES),pa_ppx_perl,ounit2,pa_ppx.utils

ML= \
	ppx_perl.ml ya_wrap_ocamlfind.ml join_meta.ml

CMO := $(ML:.ml=.cmo)

TESTS=ppx_perl.exe ya-wrap-ocamlfind join_meta

all: $(TESTS)

test:: all
	mkdir -p _build && ./ppx_perl.exe

ppx_perl.exe: ppx_perl.cmo
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

ya-wrap-ocamlfind: ya_wrap_ocamlfind.cmo
	$(OCAMLFIND) ocamlc -linkall -linkpkg -package $(PACKAGES) $< -o $@

join_meta: join_meta.cmo
	$(OCAMLFIND) ocamlc -linkall -linkpkg -package $(PACKAGES) $< -o $@

promote::
	$(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_o ya_wrap_ocamlfind.ml > ../tools/ya_wrap_ocamlfind.ml

clean::
	rm -rf *.cm* _build *.exe ya-wrap-ocamlfind join_meta

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(TESTS): $(EXTERNAL)

depend::
	echo "$(CMO) $(TESTS): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o \
	$(ML) \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend
