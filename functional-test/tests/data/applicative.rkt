#lang racket/base

(require (prefix-in c: data/collection)
         data/functor
         data/applicative
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
                    (identity 3)))))
