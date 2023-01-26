# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
OCAMLTOPLEVEL=ocaml
CAMLP5LIB=$(shell $(OCAMLFIND) query camlp5)
MKCAMLP5=mkcamlp5
MKCAMLP5OPT=mkcamlp5.opt
CAMLP5R=camlp5r -I $(CAMLP5LIB)
INCLUDES=
OCAMLCFLAGS=$(DEBUG) $(WARNERR) $(INCLUDES)
PACKAGES=camlp5,camlp5.extfun
#PACKAGES := $(PACKAGES),camlp5.parser_quotations
PACKAGES := $(PACKAGES),camlp5.quotations
SYNTAX = camlp5o
PRPACKAGE = camlp5.pr_o

export PERL5LIB := $(TOP):$(PERL5LIB)

top: all

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmo:
	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),$(PRPACKAGE) -syntax $(SYNTAX) $< > $*_ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmt:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -syntax $(SYNTAX) -bin-annot -c $< -o .cmt/$*.cmo
	cp .cmt/$*.cmt $*.cmt

.mli.cmti:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -syntax $(SYNTAX) -bin-annot -c $< -o .cmt/$*.cmi
	cp .cmt/$*.cmti $*.cmti

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti .byte .opt

clean::
	$(RM) -rf *.cm* *.o *.a *.byte *.log *.cache *ppo *ppo.ml _build/* .cmt *.odoc

realclean:: clean
