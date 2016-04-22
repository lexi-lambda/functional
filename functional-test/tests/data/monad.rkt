#lang racket/base

(require (prefix-in c: data/collection)
         data/functor
         data/applicative
         data/monad
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
     (identity (c:apply (identity-value f) (map identity-value args))))]
  #:methods gen:monad
  [(define (chain f x)
     (f (identity-value x)))])

(describe "gen:monad"
  (describe "chain"
    (it "applies a function in a monadic context"
      (check-equal? (chain (compose identity add1) (identity 11))
                    (identity 12)))))

(describe "do"
  (it "performs a sequence of chain calls"
    (check-equal? (do [x <- (identity 96)]
                      [y <- (identity (add1 x))]
                      [c <- (identity (integer->char y))]
                      (identity c))
                  (identity #\a)))

  (it "supports mutually recursive internal definitions"
    (check-equal? (do [x <- (identity 12)]
                      (define (call-b)
                        (* 2 (b)))
                      (define (b)
                        (- x))
                      [y <- (identity (call-b))]
                      (identity (add1 y)))
                  (identity -23))))

(describe "sequence"
  (describe "chain"
    (it "runs a computation through all possible paths"
      (check-equal? (c:sequence->list
                     (do [f <- (list + *)]
                         [a <- (list 1 2)]
                         [b <- (list 5 6)]
                         (pure (f a b))))
                    (list 6 7 7 8 5 6 10 12)))))

(describe "join"
  (it "flattens two levels of monadic context"
    (check-equal? (join (identity (identity 3))) (identity 3))))
