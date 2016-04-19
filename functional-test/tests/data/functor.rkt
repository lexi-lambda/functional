#lang racket/base

(require data/functor
         rackunit
         rackunit/spec)

(struct identity (value)
  #:transparent
  #:methods gen:functor
  [(define (map f x)
     (identity (f (identity-value x))))])

(describe "gen:functor"
  (describe "map"
    (it "applies a function to the values inside a context"
      (check-equal? (map add1 (identity 25))
                    (identity 26)))))
