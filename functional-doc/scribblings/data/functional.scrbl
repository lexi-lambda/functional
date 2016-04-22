#lang scribble/manual

@(require racket/require
          scribble/eval
          (for-label (except-in data/collection map)
                     (subtract-in (except-in racket/base #%app do)
                                  data/collection)
                     racket/contract
                     racket/match
                     (multi-in data [functor applicative monad maybe either])))

@(module base-ids racket/base
   (require scribble/manual (for-label racket/base))
   (provide map)
   (define map @racket[map]))

@(require (prefix-in base-id: 'base-ids))

@(define (reftech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define (make-fantasy-eval)
   (let ([eval ((make-eval-factory '()))])
     (eval '(require data/functor data/applicative data/monad data/maybe data/either
                     racket/format racket/match
                     (except-in data/collection map)))
     eval))

@(define-syntax-rule (fantasy-interaction . body)
   (interaction
    #:eval (make-fantasy-eval)
    . body))

@title{Functional generic interfaces}

@table-of-contents[]

@section[#:tag "interfaces"]{Interfaces}

@subsection[#:tag "functors"]{Functors}

@defmodule[data/functor]

A @deftech{functor} can be thought of as a kind of “container”. This can be something like a list or
hash map, which actually contains values, or something like a channel, which produces values over
time. All functors work with @racket[map], which allows producing a new functor with the elements
“contained” by the functor modified by the mapping function.

For example, using @racket[map] on lists simply modifies each element of the list, just like
@base-id:map from @racketmodname[racket/base].

@(fantasy-interaction
  (map add1 '(1 2 3)))

However, unlike @base-id:map from @racketmodname[racket/base], this more generic @racket[map] can also
map over things like @tech{optional values}.

@(fantasy-interaction
  (map add1 (just 2))
  (map add1 nothing))

Functors provide a way to manipulate data in a consistent way without needing to know the data’s
underlying structure.

@deftogether[(@defidform[#:kind "interface" gen:functor]
              @defproc[(functor? [v any/c]) boolean?]
              @defthing[functor/c contract?])]{
The @reftech{generic interface} that specifies @tech{functors}.}

@defproc[(map [f procedure?] [x functor?]) functor?]{
Applies @racket[f] to the @tech{functor} @racket[x].}

@subsection[#:tag "applicatives"]{Applicatives}

@defmodule[data/applicative]

@deftech{Applicative functors} generalize function application to work with any kind of data
structure, not just procedures. This is much like @racket[prop:procedure], but it is specified via a
generic interface. Additionally, all implementations of @racket[gen:applicative] should also implement
@racket[gen:functor].

@(fantasy-interaction
  ((just +) (just 1) (just 2))
  ((just +) nothing (just 2))
  (sequence->list
   ((list + *) (list 3 4) (list 10 20))))

In addition to the implementation of @racket[apply], the @racket[gen:applicative] interface must also
implement a function called @racket[pure]. This function “lifts” an ordinary value into the functor.
For example, the @racket[pure] function for lists is just @racket[list], but the @racket[pure]
function for optional values is @racket[just].

@deftogether[(@defidform[#:kind "interface" gen:applicative]
              @defproc[(applicative? [v any/c]) boolean?]
              @defthing[applicative/c contract?])]{
The @reftech{generic interface} that specifies @tech{applicative functors}.}

@deftogether[(@defproc[(pure [v any/c]) applicative?])]{
Lifts a plain value into an @tech{applicative functor}. When initially called, this function simply
places the value in a box because it cannot yet know what kind of functor it needs to produce. When
used, the value will be coerced into a functor of the appropriate type using the relevant value’s
@racket[pure] method.}

@subsection[#:tag "monads"]{Monads}

@defmodule[data/monad]

A @deftech{monad} is a mechanism for sequencing pure values in a context. Monads are an extremely
general concept that are notoriously difficult to explain (despite being relatively simple once you
understand them), and I will not attempt to explain them here (though perhaps I will try someday).

All monads must also be @tech{applicative functors}, but they add one more method, called
@racket[chain]. The @racket[chain] method, much like @racket[map], applies a function to a value in a
context, but unlike @racket[map], the applied function must produce a new monad, not a pure value.
Monads can be used to control sequencing of computation in a very flexible way.

Using the @racket[chain] function directly can become tedious and hard to read beyond a couple of
nested applications, so the @racket[do] form is provided to make sequencing monadic operations more
pleasant to read and write.

@deftogether[(@defidform[#:kind "interface" gen:monad]
              @defproc[(monad? [v any/c]) boolean?]
              @defthing[monad/c contract?])]{
The @reftech{generic interface} that specifies @tech{monads}.}

@defproc[(chain [f (any/c . -> . monad?)] [x monad?]) monad?]{
Applies @racket[f] to the value within the monadic context @racket[x] and produces a new monad as the
result.}

@defform[#:literals [<-]
         (do expr-or-clauses)
         #:grammar
         ([expr-or-clauses monad-expr
                           (code:line do-clause expr-or-clauses)]
          [do-clause [binding-id <- monad-expr]
                     monad-expr
                     internal-definition])
         #:contracts
         ([monad-expr monad?])]{
Syntactic shorthand for successive, nested uses of @racket[chain]. The @racket[do] form allows
writing arbitrary sequential monadic operations without an excessive proliferation of lambdas and a
significant amount of rightward drift.

In its simplest form, @racket[do] does nothing at all. Any @racket[do] block containing only a single
expression is equivalent to the expression itself.

@(fantasy-interaction
  (do 3)
  (do "hello, world")
  (do '(1 2 3 4)))

This is obviously not particularly useful, but @racket[do] becomes helpful when using multiple
sub-forms. Each @racket[do-clause] may bind the result of a @racket[chain] operation, which may be
used in subsequent computations.

@(fantasy-interaction
  (sequence->list
   (do [x <- '(1 2 3)]
       (pure (* x 2)))))

Specifically, any block of the form @racket[(do [_x <- _m] _clause ...+)] is precisely equivalent to
@racket[(chain (λ (_x) (do _clause ...+)) _m)].

Not every @racket[chain] operation has a useful result. In that case, the binding brackets may be
omitted, simply leaving the @racket[monad-expr]. In this case, a @racket[chain] call will still be
produced, but the result will not be bound anywhere.

@(fantasy-interaction
  (sequence->list
   (do '(1 2 3)
       (pure 'hello))))

Finally, arbitrary internal definitions may be interspersed between each @racket[do-clause]. These
definitions do not produce new @racket[chain] calls, they simply create new bindings.

@(fantasy-interaction
  (sequence->list
   (do [x <- '(1 2)]
       (define y (* x 2))
       [z <- '(a b)]
       (define (prettify a b) (~a a ": " b))
       (pure (prettify y z)))))

Internal defintions defined within @racket[do] blocks may refer to all previous bindings, but not
subsequent ones. However, multiple internal definitions directly next to one another may be mutually
recursive, so long as they are not separated by a @racket[chain].

@(fantasy-interaction
  (do [x <- (just 7)]
      (define (calls-b)
        (add1 (b)))
      (define (b)
        (- x))
      [y <- (just (calls-b))]
      (pure (* 2 y))))}

@deftogether[(@defidform[<-]
              @defidform[←])]{
Recognized specially within forms like @racket[do]. Using either form as an expression is a syntax
error.}

@defproc[(join [x monad?]) monad?]{
Joins a nested monadic value (a monadic value embedded within another monadic value, both of the same
type) into a single value. In other words, this @emph{flattens} a monadic value by a single layer.

@(fantasy-interaction
  (sequence->list (join '((1 2) (3 4))))
  (sequence->list (join '()))
  (join (just (just 'hello)))
  (join (just nothing))
  (join nothing))}

@section[#:tag "data-types" #:style 'toc]{Data types}

@local-table-of-contents[]

@subsection[#:tag "maybe"]{Maybe}

@defmodule[data/maybe]

The @emph{maybe} pattern implements @deftech{optional values}, values that represent computations that
can fail. Idiomatic Scheme uses @racket[#f] to represent a “lack of a value”, similar to now @tt{null}
is used in other programming languages, but this exhibits a few problems:

@itemlist[
  #:style 'ordered
  @item{Sometimes @racket[#f] can be a valid value, at which point it is ambiguous whether or not a
        result is nonexistent or if it is simply the value @racket[#f].}
  @item{Composing operations that can fail can be tedious and can result in deeply nested
        conditionals, as each step of the computation must check if the value is @racket[#f] and
        short-circuit if necessary.}]

@emph{Maybe} reifies the concept of a lack of a value as @racket[nothing] and the presence of a value
as @racket[just]. It then provides a series of combinators to help work with operations that can fail
without excessive error-checking.

Optional values are @tech{functors}, @tech{applicative functors}, and @tech{monads}. This provides a
reasonable framework for managing failable computations in a consistent and extensible way. For
example, consider an operation that can fail.

@(define maybe-eval (make-fantasy-eval))

@(interaction
  #:eval maybe-eval
  (define (safe-first lst)
    (if (empty? lst)
        nothing
        (just (first lst)))))

Now, consider using that operation on a list of characters.

@(interaction
  #:eval maybe-eval
  (safe-first '(#\a #\b #\c))
  (safe-first '()))

It is possible that you might want to, rather than retrieve the first character, get the first
character’s unicode code point. If @racket[safe-first] returned @racket[#f] rather than
@racket[nothing] upon failure, you would need to branch to check if the value was found before
attempting to convert the character to an integer.

@(racketblock
  (let ([c (safe-first list-of-chars)])
    (if c
        (char->integer c)
        #f)))

It would be possible to use @racket[and] to make things a little shorter, but the explicit
error-checking would still be necessary. However, since optional values are just @tech{functors}, it
is possible to just use @racket[map].

@(interaction
  #:eval maybe-eval
  (map char->integer (safe-first '(#\a #\b #\c)))
  (map char->integer (safe-first '())))

Consider another example: safely dividing a number without having division-by-zero errors. We can
implement a @racket[safe-/] function like we did with @racket[safe-first]:

@(interaction
  #:eval maybe-eval
  (define (safe-/ a b)
    (if (zero? b)
        nothing
        (just (/ a b)))))

Now, obviously we could use it just like we used @racket[safe-first], but what if we want to use them
both @emph{together}? That is, we want to call @racket[safe-/] on the result of @racket[safe-first].
We could try using @racket[map] again, which seems like it should work:

@(interaction
  #:eval maybe-eval
  (map (λ (x) (safe-/ 2 x))
       (safe-first '(10 20 30))))

Oops, now we have a @racket[just] wrapped inside another @racket[just]. This is because @racket[map]
replaces whatever is @emph{inside} the functor, not the functor itself, and we returned
@racket[(just 1/5)] from our mapping function. Instead, we want the inner @racket[just] to be subsumed
by the outer one. For that, we can use @racket[chain].

The @racket[chain] function works just like @racket[map], but it joins the two wrappers together into
a single wrapper after the operation is finished.

@(interaction
  #:eval maybe-eval
  (chain (λ (x) (safe-/ 2 x))
         (safe-first '(10 20 30))))

@(maybe-eval
  '(define (safe-rest lst)
     (if (empty? lst)
         nothing
         (just (rest lst)))))

We can use multiple calls to @racket[chain] to sequence many failable operations at once. For example,
we could write a function that divides the first two numbers of a list that won’t ever throw
exceptions:

@(interaction
  #:eval maybe-eval
  (define (divide-first-two lst)
    (chain
     (λ (a) (chain
             (λ (xs) (chain
                      (λ (b) (safe-/ a b))
                      (safe-first xs)))
             (safe-rest lst)))
     (safe-first lst)))
  
  (divide-first-two '(4 3 2 1))
  (divide-first-two '(5 0))
  (divide-first-two '(5))
  (divide-first-two '()))

It works! That is, itself, kinda cool. Unfortunately, following all the nested calls to @racket[chain]
will very likely make your head spin. That’s where @racket[do] comes in. The same exact function can
be rewritten using @racket[do] in a much clearer way:

@(interaction
  #:eval maybe-eval
  (define (divide-first-two lst)
    (do [a  <- (safe-first lst)]
        [xs <- (safe-rest lst)]
        [b  <- (safe-first xs)]
        (safe-/ a b)))
  (divide-first-two '(20 11))
  (divide-first-two '(3 0)))

Using the monadic interface, we can sequence arbitrary computations that can fail without writing a
single line of explicit error handling code.

@(close-eval maybe-eval)

@deftogether[(@defproc[(just [x any/c]) maybe?]
              @defthing[nothing maybe?]
              @defproc[(maybe? [v any/c]) boolean?])]{
Value constructors and predicate for @tech{optional values}. The @racket[just] function produces a
boxed value, and the @racket[nothing] value represents the absence of a value.

@(fantasy-interaction
  (just 'hello)
  nothing)

Optional values are @tech{monads} that short-circuit on @racket[nothing].

@(fantasy-interaction
  (map add1 (just 1))
  (map add1 nothing)
  ((pure +) (just 1) (just 2))
  (do [n <- (just 1)]
      (pure (add1 n))))

The @racket[nothing] binding also serves as a @reftech{match expander} that only recognizes the
@racket[nothing] value, but it must be surrounded with parentheses to be compatible with the syntax of
@racket[match].

@(fantasy-interaction
  (define/match (value-or-false mval)
    [((just val))  val]
    [((nothing))   #f ])
  (value-or-false (just 'something))
  (value-or-false nothing))}

@subsection[#:tag "either"]{Either}

@defmodule[data/either]

The @deftech{either} type provides another implementation of @tech{optional values}, generally used to
represent computations that can fail. However, it augments @racket[just] and @racket[nothing] by
allowing the @emph{kind of failure} to be annotated. When a computation results in @racket[nothing],
it clearly failed, but it is not always clear why (especially after a long chain of monadic
computation).

The @racket[right] constructor is exactly like @racket[just]—it signals a successful value, and it can
be mapped over as a @tech{functor} or @tech{applicative functor} and sequenced as a @tech{monad}. The
@racket[left] constructor has the same short-circuiting behavior of @racket[nothing], but it accepts a
value like @racket[right], which can be used to annotate the kind of failure.

As an example, we can rewrite the @racket[safe-] functions from the @seclink["maybe"]{maybe} section
using @tech{either}.

@(fantasy-interaction
  (define (safe-/ a b)
    (if (zero? b)
        (left "attempted to divide by zero")
        (right (/ a b))))

  (define (safe-first lst)
    (if (empty? lst)
        (left "attempted to get the first element of an empty list")
        (just (first lst))))

  (define (safe-rest lst)
     (if (empty? lst)
         (left "attempted to get the rest of an empty list")
         (just (rest lst))))

  (define (divide-first-two lst)
    (do [a  <- (safe-first lst)]
        [xs <- (safe-rest lst)]
        [b  <- (safe-first xs)]
        (safe-/ a b)))
  (divide-first-two '(20 11))
  (divide-first-two '(3 0))
  (divide-first-two '(3)))

@deftogether[(@defproc[(right [x any/c]) either?]
              @defproc[(left [x any/c]) either?]
              @defproc[(either? [v any/c]) boolean?])]{
Value constructors and predicate for @tech{either}, which are tagged @tech{optional values}. The
@racket[right] function produces a successful value, and the @racket[left] constructor creates a value
that represents failure.

@(fantasy-interaction
  (right 'hello)
  (left 'failed))

Either values are @tech{monads} that short-circuit on @racket[left].

@(fantasy-interaction
  (map add1 (right 1))
  (map add1 (left 'failed))
  ((pure +) (right 1) (right 2))
  (do [n <- (right 1)]
      (pure (add1 n))))}
