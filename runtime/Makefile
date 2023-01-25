# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

PACKAGES := fmt,camlp5
TARGET := pa_ppx_string_runtime.cma
ML := runtime.ml
MLI := 
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_string

all: $(TARGET) $(TARGET:.cma=.cmxa)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

pa_ppx_string.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_string.cmo $^

pa_ppx_string.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_string.cmx $^

META: META.pl
	./META.pl > META
install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_string_runtime -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_string.cmx pa_ppx_string.cmi $(wildcard *.cmt*)
	$(RM) -f META

clean::
	rm -rf META camlp5.pa_ppx_string

$(TARGET): pa_ppx_string.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) pa_ppx_string.cmo -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_string.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) pa_ppx_string.cmx -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES) -syntax camlp5o runtime.ml >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
