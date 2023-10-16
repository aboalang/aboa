# aboa

Aboa ("a good" in Portuguese) is a programming language that melds
the best aspects of other programming languages together with a few
new ideas into an alloy of surpassing quality.

## priorities (... inspired by)

1. Interactive ... APL, Lisp
2. Beautiful ... Lisp, Uiua
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

Aboa syntax is a work-in-progress, currently based on Scheme R7RS,
with the following changes and additions (* indicates not supported in Scheme):

```scheme
  Aboa  Scheme      semantics         notes

    #   ;           comment
    ^   define ( )  *function         purity is not yet enforced but will be eventually
    <   define ( )  procedure         followed by a name or _, without enclosing ( )
    (f  (f          func applic       "f" is placeholder for actual defined name, same as Scheme
    (>f (f          *proc applic      symbol name prefix is required to apply defined procedures
    _   lambda      lambda func/proc  in place of name defines a lambda, i.e. anonymous function
    ?   if          conditional       may become generalized with Scheme "cond"

    ~   define      *immutable val    may be eliminated if values are replaced with functions
    !   define      mutable var       may be eliminated if mutable state is prohibited
    &   let         block scope       may be eliminated if global scope is eliminated
    :   :           **type            *only found in Chicken Scheme and Racket
    %               **prim type       e.g. % int, .% float, etc.

    <<  <           less than         so it's 2 characters long like <=
    >>  >           greater than      so it's 2 characters long like >=
    ==  =           equal             so it's 2 characters long like !=

```
### example:

```scheme
(^ iota1 conta (map (^ _ x (+ 1 x)) (iota conta)))

(^ vetor-3d:.% x:.% y:.% z:.% (vector x y z))

(^ vetor-adição v1 v2
  (vector-map (^ _ e1 e2 (+ e1 e2)) v1 v2))

(< aleatório-le max (random (- max (random max))))

(< vetor-3d-aleatório mag
  (vector (>aleatório-le mag) (>aleatório-le mag) (>aleatório-le mag)))

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

- Enforcement between **functions** that have no side-effects (declared by
  "(^ name ...)" or (^ _ ...) for lambda) and **procedures** that have
  side-effects (declared by "(< name ...))" and called by "(>name ...)).

- No English words predefined by the syntax and no bias to English.

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

## TODO: more to be written, by c4augustus, as of 2023.10.09
