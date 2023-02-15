# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),fmt,re,pa_ppx.base,pa_ppx.deriving_plugins.std,pa_ppx_static
TARGET := pa_ppx_regexp.cma
ML := pa_regexp.ml
TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

export MIGRATE_CFLAGS= -ppopt -pa_import-I -ppopt . -ppopt -pa_passthru-debug \
	-ppopt -pa_import-package -ppopt $(PACKAGES)
export MIGRATE_PACKAGES=pa_ppx.import,pa_ppx_migrate

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_regexp camlp5.pa_ppx_regexp.opt META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_regexp: $(TARGET)
	$(MKCAMLP5) -package fmt,re,camlp5.pa_r,camlp5.pr_r,pa_ppx.base $(TARGET) -o $@

camlp5.pa_ppx_regexp.opt: $(TARGET:.cma=.cmxa)
	$(MKCAMLP5OPT) -package fmt,re,camlp5.pa_r,camlp5.pr_r,pa_ppx.base $(TARGET:.cma=.cmxa) -o $@

install:: META
	mkdir -p $(DESTDIR)/lib
	touch META
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_regexp -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) $(CMI) $(wildcard *.cmt*)

clean::
	rm -rf META camlp5.pa_ppx_regexp* mk_meta mk_meta.ml

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
