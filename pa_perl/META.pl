#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_perl" preprocessor:
requires = "camlp5,fmt,re,pa_ppx.base,pa_ppx_perl_runtime,camlp5.parser_quotations"
version = "$Version::version"
description = "pa_ppx_perl: pa_ppx_perl rewriter"

# For linking
package "link" (
requires = "camlp5,fmt,re,pa_ppx.base.link,camlp5.parser_quotations.link"
archive(byte) = "pa_ppx_perl.cma"
archive(native) = "pa_ppx_perl.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_perl.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,re,pa_ppx.base,camlp5.parser_quotations"
  archive(syntax,preprocessor,-native) = "pa_ppx_perl.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_perl.cmxa"

EOF
