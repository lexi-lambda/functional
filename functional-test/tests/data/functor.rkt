#lang racket/base

(require (except-in data/collection map)
         data/functor
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
                    (identity 26)))

    (it "works like zip when applied to sequences"
      (check-equal? (sequence->list (map + '(1 2 3) '(10 20 30))) '(11 22 33)))))
