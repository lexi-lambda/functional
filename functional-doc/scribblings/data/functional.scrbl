#lang scribble/manual

@(require racket/require
          scribble/eval
          (for-label data/collection
                     (subtract-in (except-in racket/base #%app do)
                                  data/collection)
                     (multi-in data [functor applicative monad maybe either])
                     (multi-in racket [contract format function match])))

@(module base-ids racket/base
   (require scribble/manual (for-label racket/base))
   (provide map)
   (define map @racket[map]))

@(require (prefix-in base-id: 'base-ids))

@(define (reftech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define (make-functional-eval)
   (let ([eval ((make-eval-factory '()))])
     (eval '(require data/functor data/applicative data/monad data/maybe data/either
                     (submod data/applicative custom-app)
                     data/collection racket/format racket/function racket/match))
     eval))

@(define-syntax-rule (functional-interaction . body)
   (interaction
    #:eval (make-functional-eval)
    . body))

@title{Functional generic interfaces}
@author[@author+email["Alexis King" "lexi.lambda@gmail.com"]]

This package provides a set of interfaces and data structures that are designed to make it easier to
write programs in a compositional, purely functional style. It uses @reftech{generic interfaces} via
@racketmodname[racket/generic] to provide a set of helpers that can be used with a variety of concrete
values.

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

@(functional-interaction
  (map add1 '(1 2 3)))

However, unlike @base-id:map from @racketmodname[racket/base], this more generic @racket[map] can also
map over things like @tech{optional values}.

@(functional-interaction
  (map add1 (just 2))
  (map add1 nothing))

Functors provide a way to manipulate data in a consistent way without needing to know the data’s
underlying structure. To ensure consistency and predictability, all implementations of
@racket[gen:functor] must conform to the @deftech{functor laws}, of which there are two:

@nested[
 #:style 'inset
 @itemlist[
  #:style 'ordered
  @item{@racket[(map identity _x)] must be equivalent to @racket[_x].}
  @item{@racket[(map (compose _f _g) _x)] must be equivalent to @racket[(map _f (map _g _x))].}]]

Most reasonable definitions of a functor will satisfy these laws already, but it is possible to write
an implementation that does not, and there is no guarantee that functions in this library will work
predictably on unlawful functors.

@deftogether[(@defidform[#:kind "interface" gen:functor]
              @defproc[(functor? [v any/c]) boolean?]
              @defthing[functor/c contract?])]{
The @reftech{generic interface} that specifies @tech{functors}.}

@defproc[(map [f procedure?] [x functor?]) functor?]{
Applies @racket[f] to the @tech{functor} @racket[x].}

@subsubsection[#:tag "custom-functors"]{Implementing new functors}

To define your own functors, simply implement the @racket[gen:functor] @reftech{generic interface} and
implement the @racket[map] method. The only implementation requirements are that methods conform to
their associated contracts and that they follow the @tech{functor laws}.

Here is an example implementation of the most trivial possible functor, the identity functor:

@(functional-interaction
  (struct id (val)
    #:transparent
    #:methods gen:functor
    [(define (map f x)
       (id (f (id-val x))))])
  (map add1 (id 12)))

@subsection[#:tag "applicatives"]{Applicatives}

@defmodule[data/applicative]

@deftech{Applicative functors} generalize function application to work with any kind of data
structure, not just procedures. This is much like @racket[prop:procedure], but it is specified via a
generic interface. Additionally, all implementations of @racket[gen:applicative] should also implement
@racket[gen:functor].

@(functional-interaction
  ((just +) (just 1) (just 2))
  ((just +) nothing (just 2))
  (sequence->list
   ((list + *) (list 3 4) (list 10 20))))

In addition to the implementation of @racket[apply], the @racket[gen:applicative] interface must also
implement a function called @racket[pure]. This function “lifts” an ordinary value into the functor.
For example, the @racket[pure] function for lists is just @racket[list], but the @racket[pure]
function for optional values is @racket[just].

Like functors, applicative functors have their own set of @deftech{applicative functor laws} which all
implementations of @racket[gen:applicative] must conform to:

@nested[
 #:style 'inset
 @itemlist[
  #:style 'ordered
  @item{@racket[((pure identity) _x)] must be equivalent to @racket[_x].}
  @item{@racket[(((pure compose) _f _g) _x)] must be equivalent to @racket[(_f (_g _x))].}
  @item{@racket[((pure _f) (pure _x))] must be equivalent to @racket[(pure (_f _x))].}]]

Most reasonable definitions of an applicative functor will satisfy these laws already, but it is
possible to write an implementation that does not, and there is no guarantee that functions in this
library will work predictably on unlawful applicative functors.

@deftogether[(@defidform[#:kind "interface" gen:applicative]
              @defproc[(applicative? [v any/c]) boolean?]
              @defthing[applicative/c contract?])]{
The @reftech{generic interface} that specifies @tech{applicative functors}.}

@defproc[(pure [v any/c]) applicative?]{
Lifts a plain value into an @tech{applicative functor}. When initially called, this function simply
places the value in a box because it cannot yet know what kind of functor it needs to produce. When
used, the value will be coerced into a functor of the appropriate type using the relevant value’s
@racket[pure] method.}

@defproc[(pure? [v any/c]) boolean?]{
A predicate that determines if a value is a boxed value that is awaiting coercion into a concrete type
of @tech{applicative functor}. Ideally, you should never need to use this function, but sometimes
values cannot be immediately coerced, so this can be needed.}

@defproc[(pure/c [val-ctc contract?]) contract?]{
A contract that accepts boxed values awaiting coercion into a concrete type of @tech{applicative
functor}. Ideally, you should never need to use this function, but sometimes values cannot be
immediately coerced, so this can be needed.}

@subsubsection[#:tag "custom-applicatives"]{Implementing new applicative functors}

Implementing your own applicative functors is somewhat more complicated than implementing plain
functors. You must implement two methods, named @racket[pure] and @racket[apply]. The former,
@emph{unlike} the @racket[pure] function exported by @racketmodname[data/applicative], should be a
function of @emph{two} arguments, the first of which should be ignored. This is necessary in order to
properly perform dynamic dispatch with @racketmodname[racket/generic], since some value must exist to
be dispatched on. The first argument is therefore the value being used for dispatch, but there is no
guarantee about what it is, so you should always ignore it completely.

Implementing the @racket[apply] method is somewhat more straightforward. It should be a function of
two arguments, this first corresponding to the functor in application position and second a list of
functors provided as arguments in the application.

Here is an example implementation of the most trivial possible applicative functor, the identity
functor:

@(functional-interaction
  (require (prefix-in base: racket/base))
  (struct id (val)
    #:transparent
    #:methods gen:functor
    [(define (map f x)
       (id (f (id-val x))))]
    #:methods gen:applicative
    [(define (pure _ x)
       (id x))
     (define (apply f xs)
       (base:apply (id-val f) (base:map id-val xs)))])
  ((id +) (pure 2) (pure 3)))

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

Like functors and applicative functors, monads have their own set of @deftech{monad laws} which all
implementations of @racket[gen:monad] must conform to:

@nested[
 #:style 'inset
 @itemlist[
  #:style 'ordered
  @item{@racket[(chain _f (pure _x))] must be equivalent to @racket[(_f _x)].}
  @item{@racket[(chain pure _x)] must be equivalent to @racket[_x].}
  @item{@racket[(chain (λ (y) (chain _g (_f y))) _x)] must be equivalent to
        @racket[(chain _g (chain _f _x))].}]]

Most reasonable definitions of a monad will satisfy these laws already, but it is possible to write an
implementation that does not, and there is no guarantee that functions in this library will work
predictably on unlawful monads.

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
          [do-clause [match-pattern <- monad-expr]
                     monad-expr
                     internal-definition])
         #:contracts
         ([monad-expr monad?])]{
Syntactic shorthand for successive, nested uses of @racket[chain]. The @racket[do] form allows
writing arbitrary sequential monadic operations without an excessive proliferation of lambdas and a
significant amount of rightward drift.

In its simplest form, @racket[do] does nothing at all. Any @racket[do] block containing only a single
expression is equivalent to the expression itself.

@(functional-interaction
  (do 3)
  (do "hello, world")
  (do '(1 2 3 4)))

This is obviously not particularly useful, but @racket[do] becomes helpful when using multiple
sub-forms. Each @racket[do-clause] may bind the result of a @racket[chain] operation, which may be
used in subsequent computations.

@(functional-interaction
  (sequence->list
   (do [x <- '(1 2 3)]
       (pure (* x 2)))))

Specifically, any block of the form @racket[(do [_x <- _m] _clause ...+)] is precisely equivalent to
@racket[(chain (λ (_x) (do _clause ...+)) _m)]. More generally, the binding identifier can be replaced
with a @racket[match] pattern, in which case the resulting code uses @racket[match-lambda] instead.

Not every @racket[chain] operation has a useful result. In that case, the binding brackets may be
omitted, simply leaving the @racket[monad-expr]. In this case, a @racket[chain] call will still be
produced, but the result will not be bound anywhere.

@(functional-interaction
  (sequence->list
   (do '(1 2 3)
       (pure 'hello))))

Finally, arbitrary internal definitions may be interspersed between each @racket[do-clause]. These
definitions do not produce new @racket[chain] calls, they simply create new bindings.

@margin-note{
  If a macro used within a @racket[do] block produces a @racket[begin] form containing both internal
  definitions and expressions, the whole form is spliced into the surrounding internal definition
  context. All expressions will be simply evaluated for side-effects and will not result in any
  additional calls to @racket[chain].}

@(functional-interaction
  (sequence->list
   (do [x <- '(1 2)]
       (define y (* x 2))
       [z <- '(a b)]
       (define (prettify a b) (~a a ": " b))
       (pure (prettify y z)))))

Internal definitions defined within @racket[do] blocks may refer to all previous bindings, but not
subsequent ones. However, multiple internal definitions directly next to one another may be mutually
recursive, so long as they are not separated by a @racket[chain].

@(functional-interaction
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

@(functional-interaction
  (sequence->list (join '((1 2) (3 4))))
  (sequence->list (join '()))
  (join (just (just 'hello)))
  (join (just nothing))
  (join nothing))}

@defproc[(map/m [f (any/c . -> . monad?)] [xs sequence?]) monad?]{
Applies @racket[f] to each element of @racket[xs], then chains the resulting monadic values from left
to right and returns the results as a single monadic value.

@(functional-interaction
  (define (ascii->char x)
    (if (<= 0 x 127)
        (just (integer->char x))
        nothing))
  (map/m ascii->char '(76 33))
  (map/m ascii->char '(76 -5 33)))}

@subsubsection[#:tag "custom-monads"]{Implementing new monads}

Implementing your own monads is no more complicated than implementing your own applicative functors,
you just need to provide an implementation of @racket[chain] that satisfies the @tech{monad laws}.

Here is an example implementation of the most trivial possible monad, the identity monad:

@(functional-interaction
  (require (prefix-in base: racket/base))
  (struct id (val)
    #:transparent
    #:methods gen:functor
    [(define (map f x)
       (id (f (id-val x))))]
    #:methods gen:applicative
    [(define (pure _ x)
       (id x))
     (define (apply f xs)
       (base:apply (id-val f) (base:map id-val xs)))]
    #:methods gen:monad
    [(define (chain f x)
       (f (id-val x)))])
  (do [x <- (id 1)]
      [y <- (id 2)]
      (pure (+ x y))))

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

@(define maybe-eval (make-functional-eval))

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
              @defproc[(maybe? [v any/c]) boolean?]
              @defproc[(just? [v any/c]) boolean?]
              @defproc[(nothing? [v any/c]) boolean?])]{
Value constructors and predicates for @tech{optional values}. The @racket[just] function produces a
boxed value, and the @racket[nothing] value represents the absence of a value. Optional values
can be serialized with @racketmodname[racket/serialize] (as long as any nested value is
serializable).

@(functional-interaction
  (just 'hello)
  nothing)

Optional values are @tech{monads} that short-circuit on @racket[nothing].

@(functional-interaction
  (map add1 (just 1))
  (map add1 nothing)
  ((pure +) (just 1) (just 2))
  (do [n <- (just 1)]
      (pure (add1 n))))

The @racket[nothing] binding also serves as a @reftech{match expander} that only recognizes the
@racket[nothing] value, but it must be surrounded with parentheses to be compatible with the syntax of
@racket[match].

@(functional-interaction
  (define/match (value-or-false mval)
    [((just val))  val]
    [((nothing))   #f ])
  (value-or-false (just 'something))
  (value-or-false nothing))}

@defproc[(maybe/c [val-ctc contract?]) contract?]{
Produces a contract that accepts @racket[nothing] or a @racket[just] containing a value that satisfies
@racket[val-ctc].}

@defproc[(maybe [default-value any/c] [proc (any/c . -> . any/c)] [maybe-value maybe?]) any/c]{
Performs a sort of “first-class pattern-match” on @racket[maybe-value]—if @racket[maybe-value] is
@racket[nothing], then @racket[default-value] is returned. Otherwise, if @racket[maybe-value] is
@racket[(just _x)], then the result is @racket[(proc _x)].

@(functional-interaction
  (maybe 0 add1 nothing)
  (maybe 0 add1 (just 1))
  (maybe 0 add1 (just 2)))}

@defproc[(from-just [default-value any/c] [maybe-value maybe?]) any/c]{
Equivalent to @racket[(maybe default-value identity maybe-value)]. If @racket[maybe-value] is
@racket[nothing], then the result is @racket[default-value]. Otherwise, if @racket[maybe-value] is
@racket[(just _x)], then the result is @racket[_x].

@(functional-interaction
  (from-just #f nothing)
  (from-just #f (just "hello")))}

@defproc[(from-just! [just-value just?]) any/c]{
Unwraps an @tech{optional value} if it is a @racket[just?], otherwise raises
@racket[exn:fail:contract?]. Use this function sparingly—it negates much of the benefit of using
@tech{optional values} in the first place, but sometimes there are instances in which the programmer
can prove a value will never be @racket[nothing], so this function is helpful.

@(functional-interaction
  (from-just! (just "hello"))
  (from-just! nothing))}

@defproc[(filter-just [maybes-lst (listof maybe?)]) list?]{
Given a list of @tech{optional values}, returns a new list with all of the values in the list wrapped
with @racket[just], discarding all of the values that were @racket[nothing].

@(functional-interaction
  (filter-just (list (just 1) nothing (just 3))))}

@defproc[(map-maybe [proc (any/c . -> . maybe?)] [lst list?]) list?]{
Like @racket[map] combined with @racket[filter-just], but more efficient because there is no need to
construct an intermediate list.

@(functional-interaction
  (map-maybe (λ (x) (if (positive? x) (just (sqrt x)) nothing))
             (list -2 3 0 9)))}

@defproc[(false->maybe [v any/c]) any/c]{
Produces @racket[nothing] if @racket[v] is @racket[#f], otherwise produces @racket[(just v)]. This is
useful when interacting with Racket APIs that follow the Scheme convention of using @racket[#f] as a
null value to represent failure or lack of a value.

@(functional-interaction
  (false->maybe #f)
  (false->maybe "hello"))}

@defform[(with-maybe-handler exn-pred? body ...)
         #:contracts ([exn-pred? (any/c . -> . any/c)])]{
Executes each @racket[body] form as usual, but catches any exceptions that satisfy
@racket[exn-pred?]. If such an exception is caught, the result of the whole form is @racket[nothing];
otherwise, the final @racket[body] form is evaluated to produce a value, @racket[_v], and the result
is @racket[(just _v)].

This is useful for interacting with Racket APIs that throw exceptions upon failure and adapting them
to produce @tech{optional values} instead.

@(functional-interaction
  (with-maybe-handler exn:fail:contract?
    (bytes->string/utf-8 #"\xC3"))
  (with-maybe-handler exn:fail:contract?
    (bytes->string/utf-8 #"hello")))}

@defproc[(exn->maybe [exn-pred? (any/c . -> . any/c)] [proc procedure?] [arg any/c] ...) maybe?]{
A procedure version of @racket[with-maybe-handler] that functions like @racket[apply], except that any
exceptions thrown during the dynamic extent of the call that match @racket[exn-pred?] will cause the
entire expression to evaluate to @racket[nothing]. Otherwise, the result is wrapped in @racket[just]
and return as-is.

This can be especially useful when paired with @racket[curry], which can be used to produce a wrapped
version of a procedure that throws exceptions that instead reports failures in terms of
@tech{optional values}.

@(functional-interaction
  (define try-bytes->string/utf-8
    (curry exn->maybe exn:fail:contract? bytes->string/utf-8))
  (try-bytes->string/utf-8 #"\xC3")
  (try-bytes->string/utf-8 #"hello"))}

@subsection[#:tag "either"]{Either}

@defmodule[data/either]

The @deftech{either} type provides another implementation of @tech{optional values}, generally used to
represent computations that can fail. However, it augments @racket[just] and @racket[nothing] by
allowing the @emph{kind of failure} to be annotated. When a computation results in @racket[nothing],
it clearly failed, but it is not always clear why (especially after a long chain of monadic
computation).

The @racket[success] constructor is exactly like @racket[just]—it signals a successful value, and it
can be mapped over as a @tech{functor} or @tech{applicative functor} and sequenced as a @tech{monad}.
The @racket[failure] constructor has the same short-circuiting behavior of @racket[nothing], but it
accepts a value like @racket[success], which can be used to annotate the kind of failure.

As an example, we can rewrite the @racket[safe-] functions from the @seclink["maybe"]{maybe} section
using @tech{either}.

@(functional-interaction
  (define (safe-/ a b)
    (if (zero? b)
        (failure "attempted to divide by zero")
        (success (/ a b))))

  (define (safe-first lst)
    (if (empty? lst)
        (failure "attempted to get the first element of an empty list")
        (success (first lst))))

  (define (safe-rest lst)
     (if (empty? lst)
         (failure "attempted to get the rest of an empty list")
         (success (rest lst))))

  (define (divide-first-two lst)
    (do [a  <- (safe-first lst)]
        [xs <- (safe-rest lst)]
        [b  <- (safe-first xs)]
        (safe-/ a b)))
  (divide-first-two '(20 11))
  (divide-first-two '(3 0))
  (divide-first-two '(3)))

@deftogether[(@defproc[(success [x any/c]) either?]
              @defproc[(failure [x any/c]) either?]
              @defproc[(either? [v any/c]) boolean?]
              @defproc[(success? [v any/c]) boolean?]
              @defproc[(failure? [v any/c]) boolean?])]{
Value constructors and predicates for @tech{either}, which are tagged @tech{optional values}. The
@racket[success] function produces a successful value, and the @racket[failure] constructor creates a
value that represents failure. Success and failure values can be serialized using
 @racketmodname[racket/serialize] as long as the inner values are serializable.

@(functional-interaction
  (success 'hello)
  (failure 'failed))

Either values are @tech{monads} that short-circuit on @racket[failure].

@(functional-interaction
  (map add1 (success 1))
  (map add1 (failure 'failed))
  ((pure +) (success 1) (success 2))
  (do [n <- (success 1)]
      (pure (add1 n))))}

@defproc[(either/c [failure-ctc contract?] [success-ctc contract?]) contract?]{
Produces a contract that accepts @tech{either} values. If the value is a @racket[failure], the
contained value must satisfy @racket[failure-ctc]; likewise, if the value is a @racket[success], it
must satisfy @racket[success-ctc].}

@defproc[(either [failure-proc (any/c . -> . any/c)] [success-proc (any/c . -> . any/c)]
                 [either-value maybe?])
         any/c]{
Like @racket[maybe] for @tech{either} values, performs a sort of “first-class pattern-match” on
@racket[either-value]—if @racket[either-value] is @racket[(failure _x)], then the result is
@racket[(failure-proc _x)]. Otherwise, if @racket[either-value] is @racket[(success _x)], then the
result is @racket[(success-proc _x)].

@(functional-interaction
  (either string-length add1 (failure "failed"))
  (either string-length add1 (success 1))
  (either string-length add1 (success 2)))}

@defproc[(from-success [default-value any/c] [either-value either?]) any/c]{
Equivalent to @racket[(either (const default-value) identity either-value)]. If @racket[either-value]
is a @racket[failure?], then the result is @racket[default-value]. Otherwise, if @racket[either-value]
is @racket[(success _x)], then the result is @racket[_x].

@(functional-interaction
  (from-success #f (failure "failed"))
  (from-success #f (success 18)))}

@defproc[(from-failure [default-value any/c] [either-value either?]) any/c]{
Equivalent to @racket[(either identity (const default-value) either-value)], which is also just
@racket[from-success] with the sides flipped. If @racket[either-value] is a @racket[success?], then
the result is @racket[default-value]. Otherwise, if @racket[either-value] is @racket[(failure _x)],
then the result is @racket[_x].

@(functional-interaction
  (from-failure #f (failure "failed"))
  (from-failure #f (success 18)))}

@defproc[(from-either [either-value either?]) any/c]{
Extracts the value from any @tech{either} value; equivalent to
@racket[(either identity identity either-value)]. If @racket[either-value] is @racket[(success _x)],
then the result is @racket[_x]. Otherwise, if @racket[either-value] is @racket[(failure _y)], then the
result is @racket[_y].

@(functional-interaction
  (from-either (failure "failed"))
  (from-either (success 18)))}

@defproc[(map-failure [f (any/c . -> . any/c)] [e either?]) either?]{
Like @racket[map] over @tech{either} values, but flipped: it applies @racket[f] to values inside of a
@racket[failure] instead of a @racket[success].

@(functional-interaction
  (map-failure symbol->string (success 1))
  (map-failure symbol->string (failure 'failed)))}

@defproc[(flip-either [e either?]) either?]{
Converts @racket[success]es into @racket[failure]s and vice-versa.

@(functional-interaction
  (flip-either (success 'foo))
  (flip-either (failure 'bar)))}

@defproc[(maybe->either [x any/c] [m maybe?]) either?]{
Converts @racket[m] to an @tech{either} value. A @racket[just] is converted to a @racket[success]
containing the same value, and a @racket[nothing] is converted to a @racket[failure] containing
@racket[x].

@(functional-interaction
  (maybe->either 'fail (just 42))
  (maybe->either 'fail nothing))}

@defproc[(either->maybe [e either?]) maybe?]{
Converts @racket[e] to an unannotated @tech{optional value}. A @racket[success] is converted to a
@racket[just] containing the same value, and a @racket[failure] is converted to @racket[nothing].

@(functional-interaction
  (either->maybe (success 42))
  (either->maybe (failure 'fail)))}
