# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := fmt,camlp5,pcre
TARGET := pa_ppx_regexp_runtime.cma
ML := runtime.ml
TOOLSML = mk_meta.ml
MLI := 
ALLSRC=$(ML) $(MLI) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_regexp

all: $(TARGET) $(TARGET:.cma=.cmxa) META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

pa_ppx_regexp.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_regexp.cmo $^

pa_ppx_regexp.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_regexp.cmx $^

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_regexp_runtime -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_regexp.cmx pa_ppx_regexp.cmi $(wildcard *.cmt*)

clean::
	rm -rf META mk_meta mk_meta.ml

$(TARGET): pa_ppx_regexp.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) pa_ppx_regexp.cmo -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_regexp.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) pa_ppx_regexp.cmx -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
