WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

SYNTAX := camlp5o

PACKAGES := $(PACKAGES),pa_ppx_regexp,ounit2,pa_ppx.utils

ML= \
	pa_ppx_regexp_test.ml ya_wrap_ocamlfind.ml join_meta.ml syntax_test.ml

CMO := $(ML:.ml=.cmo)

TESTS=pa_ppx_regexp_test.exe syntax_test.exe ya-wrap-ocamlfind join_meta

all: $(TESTS)

test:: all
	mkdir -p _build && ./pa_ppx_regexp_test.exe
	mkdir -p _build && ./syntax_test.exe

toplevel::
	$(LAUNCH) ocaml -nopromptcont

pa_ppx_regexp_test.exe: pa_ppx_regexp_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

syntax_test.exe: syntax_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pa_op.link,camlp5.pr_o.link,str,pa_ppx.base.link,pa_ppx_regexp.link,pa_ppx.testutils -linkpkg -linkall -o $@ $<

ya-wrap-ocamlfind: ya_wrap_ocamlfind.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg -package $(PACKAGES) $< -o $@

join_meta: join_meta.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg -package $(PACKAGES) $< -o $@

clean::
	rm -rf *.cm* _build *.exe ya-wrap-ocamlfind join_meta

EXTERNAL := $(shell $(LAUNCH) $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(TESTS): $(EXTERNAL)

depend::
	echo "$(CMO) $(TESTS): $(EXTERNAL)" > .depend.NEW
	$(LAUNCH) $(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o \
	$(ML) \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend
