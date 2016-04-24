#lang curly-fn racket/base

(require (prefix-in c: data/collection)
         racket/generic
         racket/list
         static-rename)

(provide gen:functor functor? functor/c
         (rename-out [variadic-map map]))

(define-generics functor
  (map f functor)
  #:defaults
  ([c:sequence? (define map c:map)]))

(define/renamed map (variadic-map f . args)
  (if (c:sequence? (first args))
      (apply c:map f args)
      (apply map f args)))
