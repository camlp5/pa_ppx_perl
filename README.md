
Some notes on these extensions


## Match regexps

```
[%match {|...re...|}]
```

type:
```
string -> result
```

Modifiers

`i`: case-insensitive matching
`opt`: use option for match-failure instead of exception

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
[%pattern expr]
```

type: `Re.Group.t -> string`

In the first case, the pattern can contain $N or ${N} (where N is an
integer) and these will be replaced with calls to the N-th capture
group (where None gets mapped to the emptry string).  Instances of
${...} are treated as antiquotations and mapped to expressions.

In the second case, the expression can contain $N$ (where N is an
integer) and these are treated as $N above.  In all other cases, $...$
is handled as an antiquotation.

So both syntaxes support both capture-variables ($N/${N} vs. $N$) and
antiquotations (${...} vs $....$)

## Match-and-replace

```
[%replace {|...re...|} / {|...pattern...|}]
```

```
[%replace {|...re...|} / expr]
```

type: `string -> string`

This combines match-regexps and substitutions-patterns.

Modifiers

`i` : case-insensitive matching
`g` : replace all matches
