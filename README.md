# aboa

Aboa ("a good" in Portuguese) is a programming language that melds
the best aspects of other programming languages together with a few
new ideas into an alloy of surpassing quality.

## priorities (... inspired by)

1. Interactive ... APL, Lisp
2. Beautiful ... APL, Lisp
3. Ubiquitous ... C, Python
4. Uncompromising ... Haskell, Rust
5. Minimal ... Erlang, Scheme
6. Superb ... BQN, Idris, LFE, Racket

## non-goals

- Monetization
- Accomodation
- Mass adoption
- Ease of learning
- Code generation
- Packaging

## syntax WIP

Aboa syntax is a work-in-progress, currently based on Scheme RSR7,
with the following changes and additions (* indicates not supported in Scheme):

```scheme
  Aboa  Scheme   semantics     notes

    #   ;        comment
    ~   define   *immutable    may become only for functions instead of vals, purity is not yet enforced
    !   define   mutable       may become only for procedures instead of vars
    ^   lambda   *function     purity is not yet enforced
    &   lambda   procedure
    @   let      block scope   may become only for funcs/procs or be eliminated
    ?   if       conditional   may become generalized with Scheme "cond"

```

## notable features

- Functional programming paradigm only; no object-orientation.

- All special forms/reserved keywords are symbols, never words. Words
  are used exclusively by user/library definitions and can always be
  redefined whereas symbols cannot.

- Parentheses ( ) like in Lisp to achieve data-as-code, clearly establish
  syntax boundaries (i.e not relying upon indentation or line-breaks),
  and make code appear obviously as code when read.

- Pipe operator | like that in Elixir, F#, Idris, etc., that is missing
  from Scheme (only in the "chain" SRFI and Racket "threading" macros,
  both lousy names).

- No brackets or curly braces because they can be difficult to type
  on international keyboards. Curly braces in particular mark a
  programming language as a descendent from C, which Aboa is not.

- Comments start with # (hash/pound) because it is the most readable
  and common (Elixir, F#, Julia, Perl, Python, R, Ruby, Shell, Tcl).

- Top of all source files declare a language+version for its syntax,
  akin to #lang in Racket.

- No package management or pre-built distribution. Reuse is assumed
  to be achieved by inclusion of other sources, however they are
  acquired (e.g. git, tarball) and arranged (e.g. copied, symlinked).

## distinquishing (possibly unique?) features

- No English words predefined by the syntax and no bias to English.

- Enforcement between **functions** that have no side-effects (declared by
  "(. (name ...))") and **procedures** that have side-effects (declared by
  "(! (name ...))", called by "(!name ...)).

## implementations - all support identical syntax:

- [aboa-s7](code/aboa-s7) embeddable C
  * derived from [s7](https://github.com/aboalang/s7-patched)

- on the [Erlang BEAM](https://github.com/aboalang/otp)
  * plan to derive from [LFE](https://github.com/aboalang/LFE)

- within its own VM
  * plan to derive from [Chez Scheme](https://github.com/aboalang/ChezScheme)
or [Racket](https://github.com/aboalang/racket)

- TODO: possibly? [Racket #lang](https://docs.racket-lang.org/guide/hash-languages.html)

- TODO: possibly? web via [Wasm](https://webassembly.org) ...

## tool support

- [aboa-vim](code/aboa-vim) ViM configuration for the aboa syntax and color scheme

## TODO: more to be written, by c4augustus, as of 2023.10.02
