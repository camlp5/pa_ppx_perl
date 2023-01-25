#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_string_runtime" package:
requires = "fmt"
version = "$Version::version"
description = "pa_ppx_string runtime support"

# For linking
archive(byte) = "pa_ppx_string_runtime.cma"
archive(native) = "pa_ppx_string_runtime.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_string_runtime.cma"

EOF
