# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile

WD=$(shell pwd)
DESTDIR=
RM=rm

SYSDIRS= runtime pa_perl

TESTDIRS= tests

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

test: all
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

mdx-test::
	$(LAUNCH) ocaml-mdx test README.asciidoc
	test -f README.asciidoc.corrected && diff -Bwiu README.asciidoc README.asciidoc.corrected

META: all
	$(JOINMETA) -rewrite pa_ppx_perl_runtime:pa_ppx_perl.runtime \
			-direct-include pa_perl \
			-wrap-subdir runtime:runtime > META

install: META
	$(OCAMLFIND) remove pa_ppx_perl || true
	$(OCAMLFIND) install pa_ppx_perl META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx_perl || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install $(BATCHTOP) META

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
