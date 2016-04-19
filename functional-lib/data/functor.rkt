#lang curly-fn racket/base

(require (prefix-in c: data/collection)
         racket/contract
         racket/generic)

(provide gen:functor functor? functor/c map
         (contract-out [ignore (functor? . -> . functor?)]))

(define-generics functor
  (map f functor)
  #:defaults
  ([c:sequence? (define map c:map)]))

(define ignore #{map void})
