
Some notes on these extensions


## Match regexps

```
[%match {|...re...|}]
```

type:
```
string -> (string option) tuple
```

```
[%match {|...re...|} / i]
```
case-insensitive matching

the type of tuple has the same # of components as the # of capture groups in the regexp

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
string -> [`Text of string | `Delim of (string option) tuple]
```

The tuple is as in match regexps.

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
