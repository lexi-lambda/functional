#lang curly-fn racket/base

(require racket/require
         racket/serialize
         (prefix-in c: data/collection)
         (multi-in data [functor applicative monad maybe])
         (multi-in racket [contract generic match serialize])
         (for-syntax racket/base
                     syntax/parse))

(provide either? success success? failure failure? either/c
         (contract-out
          [either ((any/c . -> . any/c) (any/c . -> . any/c) either? . -> . any/c)]
          [from-success (any/c either? . -> . any/c)]
          [from-failure (any/c either? . -> . any/c)]
          [from-either (either? . -> . any/c)]
          [map-failure ((any/c . -> . any/c) either? . -> . either?)]
          [either->maybe (either? . -> . maybe?)]
          [maybe->either (any/c maybe? . -> . either?)]
          [flip-either (either? . -> . either?)]))

(define (either? x)
  (or (success? x) (failure? x)))

(serializable-struct success (value)
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

(serializable-struct failure (value)
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

(define/match (either g f m)
  [(_ f (success x)) (f x)]
  [(g _ (failure y)) (g y)])

(define/match (from-success x m)
  [(_ (success x)) x]
  [(x (failure _)) x])

(define/match (from-failure x m)
  [(x (success _)) x]
  [(_ (failure x)) x])

(define/match (from-either x)
  [((success x)) x]
  [((failure x)) x])

(define/match (map-failure f x)
  [(_ (success x)) (success x)]
  [(f (failure x)) (failure (f x))])

(define/match (flip-either x)
  [((success x)) (failure x)]
  [((failure x)) (success x)])

(define (either->maybe x)
  (either nothing just x))
(define (maybe->either e x)
  (maybe (failure e) success x))
