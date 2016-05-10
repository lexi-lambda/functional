#lang curly-fn racket/base

(require racket/require
         (prefix-in c: data/collection)
         (multi-in data [functor applicative monad])
         (multi-in racket [contract generic match])
         (for-syntax racket/base
                     syntax/parse))

(provide either? success success? failure failure? either/c
         either from-either map-failure)

(define (either? x)
  (or (success? x) (failure? x)))

(struct success (value)
  #:transparent
  #:methods gen:functor
  [(define (map f x)
     (success (f (success-value x))))]
  #:methods gen:applicative
  [(define (pure _ x) (success x))
   (define/contract (apply f args)
     (any/c (listof either?) . -> . any/c)
     (or (findf failure? args)
         (success (c:apply (success-value f) (map success-value args)))))]
  #:methods gen:monad
  [(define (chain f x)
     (f (success-value x)))])

(struct failure (value)
  #:transparent
  #:methods gen:functor
  [(define (map f x) x)]
  #:methods gen:applicative
  [(define (pure _ x) (success x))
   (define (apply f args) f)]
  #:methods gen:monad
  [(define (chain f x) x)])

(define (either/c failure/c success/c)
  (or/c (struct/c failure failure/c)
        (struct/c success success/c)))

(define/match (either x f m)
  [(_ f (success x)) (f x)]
  [(x _ (failure _)) x])

(define/match (from-either x m)
  [(_ (success x)) x]
  [(x (failure _)) x])

(define/match (map-failure f x)
  [(_ (success x)) (success x)]
  [(f (failure x)) (failure (f x))])
