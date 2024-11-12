# aboa

Aboa ("the good" in Portuguese) is a programming language that melds
the best aspects of other programming languages together with a few
new ideas into an alloy of surpassing quality.

## priorities (... inspired by)

1. Interactive ... Lisp, Smalltalk
2. Beautiful ... APL, Uiua
3. Ubiquitous ... C, Python
4. Uncompromising ... Haskell, Rust
5. Minimal ... Erlang, Scheme
6. Sublime ... BQN, Idris, LFE, Racket

## non-goals

- Monetization
- Accomodation
- Mass adoption
- Ease of learning
- Code generation
- Packaging

## syntax WIP

Aboa syntax is a work-in-progress, previously based on Scheme R7RS,
but now deviating quite far away from Lisps, with the following differences:

1.  Like Scheme, there are only expressions, no statements whatsoever.
    The topmost level is a single expression that contains 0 or more
    nested expressions within.

2.  Unlike Scheme, not all expressions must be bounded by lists (...).

3.  Also unlike Scheme, every expression is essentially a function that
    takes one argument which is always a list of 0 (empty), 1, or more values,
    and produces a single list of 0, 1, or more values.

4.  Even more unlike Scheme--and most other languages--expressions
    and their values within are evaluated from left to right, top to bottom,
    one after the other in a pipe-like sequence, the result of each previous
    expression becoming the argument to the next expression to its right, or
    below on the left side of the next line. Thus nested expressions, are
    neither evaluated nor executed until they are encountered. This supports
    a single pass during interpretation.

```scheme
    aboa        Scheme            semantics

    #           ;                 comment
    _           argname           expression input argument reference, single list in aboa, one or more in Scheme
    _n          argname           nth list element of input argument reference
    :           '()               empty value
    ~                             dyadic catenate L to R
    (...)       (...)             bounds of nesting
    >           (...)             apply function  (pure),      left to right
    >*          (...)             apply procedure (effectful), left to right
    (^ ...)                       declare anon function  (pure)
    (* ...)     (lambda ...)      declare anon procedure (effectful)
    name(                         beginning of named expression that serves as comment or point of reference
    )name                         end of named expression, required when its beginning is named
    name(^ ...                    define named function  (pure)
    name(* ...  define (name ...  define named procedure (effectful)
    >name                         apply named function, its one argument comes from its left
    >*name      (p ...)           apply named procedure, in aboa its one argument comes from its left
    _           argname           argument reference, single list in aboa, one or more in Scheme
    <)          (p ...)           tail recursion to beginning of func/proc

    $                             standard library name prefix

    ?           if                conditional, may become generalized with Scheme "cond"

    ==          =                 equal             so it's 2 characters long like !=
    /=          !=                not equal
    <=          <=                less than or equal
    <<          <                 less than         so it's 2 characters long like <=
    >=          >=                greater than or equal
    >>          >                 greater than      so it's 2 characters long like >=

    :i          ...->integer      dyadic convert to integer

    ! ...                         on fail apply

    OLD SCHEME EQUIVALENT SYNTAX:

    &           fold        block scope       may be eliminated if global scope is eliminated
    :           :           **type            *only found in Chicken Scheme and Racket
    %                       **prim type       e.g. % int, .% float, "" string, etc.


```
### examples:

```scheme
#!/usr/bin/env aboa
#:aboa-v-00-01
# Bye Bye Hello World
>(=[] ? ("countdown: " >$io.si) _
  %i ! ("Invalid countdown "~_~", try again...\n" >$io.sof [] <))
("World, Hello..." >$io.sof _)
..0 & (_1~"..." >$io.sof 1 >$cc.sleep)
"Bye Bye.\n" >$io.sof
```

## notable features

- Functional programming paradigm only; no object-orientation.

- All special forms/reserved keywords are symbols, never words. Words
  are used exclusively by user/library definitions and can always be
  redefined whereas symbols cannot.

- Parentheses ( ) like in Lisp to achieve data-as-code, clearly establish
  syntax boundaries (i.e not relying upon indentation or line-breaks),
  and make code appear obviously as code when read.

- No curly braces because they can be difficult to type on international
  keyboards and mark a programming language as a descendent from C,
  which Aboa is not.

- Comments start with # (hash/pound) because it is the most readable
  and common (Elixir, F#, Julia, Perl, Python, R, Ruby, Shell, Tcl).

- Top of all source files (after #! if present) declare a language+version
  for its syntax, akin to #lang in Racket.

- No package management or pre-built distribution. Reuse is assumed
  to be achieved by inclusion of other sources, however they are
  acquired (e.g. git, tarball) and arranged (e.g. copied, symlinked, submodule).

## distinquishing (possibly unique?) features

- Enforcement between **functions** that have no side-effects (declared by
  name(^ ...) or (^ ...) for lambda) and **procedures** that have
  side-effects (declared by name(> ...)) and called by >name(name ...).

- No English words predefined by the syntax, although the standard library is.

## implementations - all support identical syntax:

- WIP: [aboa-s7](code/plat/s7) embeddable C
  * derived from [s7](https://github.com/aboalang/s7)

- WIP: [Racket](https://github.com/aboalang/racket) [#lang](code/plat/racket/aboa.rkt)

- WIP: [POSIX shell](code/plat/sh/aboa)

- TODO: on the [Erlang BEAM](https://github.com/aboalang/otp)
  * plan to derive from [LFE](https://github.com/aboalang/LFE)

- TODO: within its own VM
  * plan to derive from [Chez Scheme](https://github.com/aboalang/ChezScheme)

- TODO: possibly? web via [Wasm](https://webassembly.org) ...

## tool support

- [aboa-vim](code/supp/vim) ViM configuration for the aboa syntax and color scheme

## TODO: more to be written, by c4augustus, as of 2024.05.24
