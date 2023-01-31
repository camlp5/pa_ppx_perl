# `pa_ppx_perl`: a PPX Rewriter for Perl-ish regexp operations


## Match regexps

```
[%match {|...re...|}]
```

type:
```
string -> result
```

Modifiers

`i`:: case-insensitive matching
`exc`:: use exception match-failure instead option
`group`:: return the `Re.Group.t` object
`strings`:: [DEFAULT] return tuple of `string option` components for capture group

The type of result varies depending whether we're using exceptions for
match-failure or not.

If using exceptions, then the type of result is:
```
string * string option * ... * string option
```

where the # of `string option` corresponds to the # of actual capture groups in the regexp.

If using option, then the type of result is as above, but wrapped in an `option`.
For a regexp without any captures, this becomes `string option`

## Split

```
[%split {|re-without-captures|}]
```

type: `string -> string list`

```
[%split {|re-with-captures|}]
```

type: 
```
string -> [`Text of string | `Delim of result]
```

The result is as in match regexps.

## Substitution Patterns

```
[%pattern {|...pattern...|}]
```

```
[%pattern {|...expr...|} / e]
```

type: if the pattern uses any capture-group expressions (viz. `$1`, or `$1$`) then `Re.Group.t -> string`
else `string`.

In the first case, the pattern can contain $N or ${N} (where N is an
integer) and these will be replaced with calls to the N-th capture
group (where None gets mapped to the emptry string).  Other instances
of ${...} are treated as antiquotations and mapped to expressions.

In the second case, the expression is within a string, and can contain
$N$ (where N is an integer) and these are treated as $N above.  The
expression is parsed by the current parser, then that AST is used for
the r.h.s. of the pattern.

So both syntaxes support both capture-variables ($N/${N} vs. $N$) and
antiquotations (${...} vs plain expressions)

## Match-and-replace

```
[%subst {|...re...|} / {|...pattern...|}]
```

```
[%subst {|...re...|} / {|...pattern...|} / ...modifiers...]
```

type: `string -> string`

This combines match-regexps and substitutions-patterns, and should be
understood as like the perl

```
s/re/pat/
```

Modifiers

`i` :: case-insensitive matching
`g` :: replace all matches
`e` :: pattern is an OCaml expression
