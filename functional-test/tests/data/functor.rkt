#lang racket/base

(require (except-in data/collection map)
         (prefix-in b: racket/base)
         data/functor
         racket/generic
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
                    (identity 26))
      (check-equal? (sequence->list (map add1 (list 1 2 3)))
                    (list 2 3 4)))

    (it "works like zip when applied to sequences"
        (check-equal? (sequence->list (map + '(1 2 3) '(10 20 30))) '(11 22 33)))

    (it "uses a functor map specification, if available, when applied to a sequence"
        (struct bag (items)
          #:transparent

          #:methods gen:functor
          [(define (map f x)
             ;; exclude numbers too big to fit in the bag
             (bag (b:filter (lambda (v)
                              (< v 10))
                            (b:map f (bag-items x)))))]

          #:methods gen:sequence
          [(define/generic -empty? empty?)
           (define/generic -first first)
           (define/generic -rest rest)
           (define (empty? x)
             (-empty? (bag-items x)))
           (define (first x)
             (-first (bag-items x)))
           (define (rest x)
             (bag (-rest (bag-items x))))])

        (check-equal? (map add1 (bag (list 7 8 9))) (bag (list 8 9))))))
