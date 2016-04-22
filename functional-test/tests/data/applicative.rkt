#lang racket/base

(require (prefix-in c: data/collection)
         data/functor
         (rename-in data/applicative [#%app #%app-applicative])
         racket/contract
         rackunit
         rackunit/spec)

(define (indirected-identity? x)
  (identity? x))

(struct identity (value)
  #:transparent
  #:methods gen:functor
  [(define (map f x)
     (identity (f (identity-value x))))]
  #:methods gen:applicative
  [(define (pure _ x) (identity x))
   (define/contract (apply f args)
     (indirected-identity? (listof indirected-identity?) . -> . indirected-identity?)
     (identity (c:apply (identity-value f) (map identity-value args))))])

(describe "gen:applicative"
  (describe "pure"
    (it "wraps a plain value in a context"
      (define (pure-wrapper->identity x)
        ((identity values) x))
      (check-equal? (pure-wrapper->identity (pure 3))
                    (identity 3))))

  (describe "apply"
    (it "applies a function in a context to values in a context"
      (check-equal? ((identity +) (identity 1) (identity 2))
                    (identity 3)))

    (it "determines the instance from the arguments if the function is pure"
      (check-equal? ((pure +) (identity 1) (identity 2))
                    (identity 3)))))

(describe "sequence"
  (describe "pure"
    (it "wraps a single value in a sequence"
      (check-equal? (c:sequence->list (#%app-applicative (list values) (pure 3)))
                    (list 3))))

  (describe "apply"
    (it "applies a function to all combinations of values"
      (check-equal? (c:sequence->list (#%app-applicative (list + *) (list 1 2) (list 3 4)))
                    (list 4 5 5 6 3 4 6 8)))))
