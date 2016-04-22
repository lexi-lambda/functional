#lang curly-fn racket/base

(require racket/require
         (prefix-in c: data/collection)
         (multi-in data [functor applicative monad])
         (multi-in racket [contract generic match])
         (for-syntax racket/base
                     syntax/parse))

(provide either? right right? left left?
         either from-either)

(define (either? x)
  (or (right? x) (left? x)))

(struct right (value)
  #:transparent
  #:methods gen:functor
  [(define (map f x)
     (right (f (right-value x))))]
  #:methods gen:applicative
  [(define (pure _ x) (right x))
   (define/contract (apply f args)
     (any/c (listof either?) . -> . any/c)
     (or (findf left? args)
         (right (c:apply (right-value f) (map right-value args)))))]
  #:methods gen:monad
  [(define (chain f x)
     (f (right-value x)))])

(struct left (value)
  #:transparent
  #:methods gen:functor
  [(define (map f x) x)]
  #:methods gen:applicative
  [(define (pure _ x) (right x))
   (define (apply f args) f)]
  #:methods gen:monad
  [(define (chain f x) x)])

(define/match (either x f m)
  [(_ f (right x)) (f x)]
  [(x _ (left _))  x])

(define/match (from-either x m)
  [(_ (right x)) x]
  [(x (left _))  x])
