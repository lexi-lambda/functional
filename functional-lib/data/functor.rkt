#lang curly-fn racket/base

(require (prefix-in c: data/collection)
         racket/contract
         racket/generic)

(provide gen:functor functor? functor/c map)

(define-generics functor
  (map f functor)
  #:defaults
  ([c:sequence? (define map c:map)]))
