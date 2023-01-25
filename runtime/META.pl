#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_perl_runtime" package:
requires = "fmt"
version = "$Version::version"
description = "pa_ppx_perl runtime support"

# For linking
archive(byte) = "pa_ppx_perl_runtime.cma"
archive(native) = "pa_ppx_perl_runtime.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_perl_runtime.cma"

EOF
