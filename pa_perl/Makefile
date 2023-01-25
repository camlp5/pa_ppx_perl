# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

PACKAGES := $(PACKAGES),fmt,re,pa_ppx.base
TARGET := pa_ppx_perl.cma
ML := pa_perl.ml
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_perl camlp5.pa_ppx_perl.opt
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_perl: $(TARGET)
	$(MKCAMLP5) -package fmt,re,camlp5.pa_r,camlp5.pr_r,pa_ppx.base $(TARGET) -o $@

camlp5.pa_ppx_perl.opt: $(TARGET:.cma=.cmxa)
	$(MKCAMLP5OPT) -package fmt,re,camlp5.pa_r,camlp5.pr_r,pa_ppx.base $(TARGET:.cma=.cmxa) -o $@

META: META.pl
	./META.pl > META

install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_perl -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) $(wildcard *.cmt*)
	$(RM) -f META

clean::
	rm -rf META camlp5.pa_ppx_perl*

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o *.ml *.mli >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
