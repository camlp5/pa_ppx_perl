# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile

WD=$(shell pwd)
DESTDIR=
RM=rm

SYSDIRS= runtime pa_regexp

TESTDIRS= tests

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

test: all mdx-test
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

mdx-test:: README.asciidoc.TEST

README.asciidoc.TEST: README.asciidoc.corrected README.asciidoc
	perl -p -i -e 's,.*: added to search path,: added to search path,' README.asciidoc.corrected
	perl -p -i -e 's,.*: loaded,: loaded,' README.asciidoc.corrected
	diff -Bwiu $^ || true

META: all
	$(JOINMETA) -rewrite pa_ppx_regexp_runtime:pa_ppx_regexp.runtime \
			-direct-include pa_regexp \
			-wrap-subdir runtime:runtime > META

install: META
	$(OCAMLFIND) remove pa_ppx_regexp || true
	$(OCAMLFIND) install pa_ppx_regexp META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx_regexp || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install $(BATCHTOP) META *.corrected

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
