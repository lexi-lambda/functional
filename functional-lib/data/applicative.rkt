#lang racket/base

(require (prefix-in c: data/collection)
         (prefix-in r: racket/base)
         racket/contract
         racket/function
         racket/generic
         (only-in "functor.rkt" gen:functor)
         "monad.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide gen:applicative applicative? applicative/c
         pure/c
         (rename-out [delayed-pure pure]
                     [delayed-pure? pure?]))

(require racket/trace)

(module+ coerce-delayed
  (provide coerce-pure))

(module+ custom-app
  (provide (rename-out [#%app-applicative #%app])))

(define-generics applicative
  (apply applicative args)
  (pure applicative value)
  
  #:derive-property prop:procedure
  (λ (f . args) (apply-applicative f args))

  #:defaults
  ([c:sequence?
    (define (pure _ x) (list x))
    (define (apply fs args)
      (c:for*/sequence ([f (c:in fs)]
                        [xs (c:in (c:apply c:cartesian-product args))])
        (c:apply f xs)))]))

; given two applicative instances, the second of which might be delayed, coerce the second instance to
; be an instance of the first using its pure implementation
; applicative? -> applicative? -> applicative?
(define ((coerce-pure applicative) delayed)
  (if (delayed-pure? delayed)
      (pure applicative (delayed-pure-value delayed))
      delayed))

; a wrapper function around the apply method for unwrapping delayed pure values
; any/c list? -> any
(define (apply-applicative f args)
  (cond [(applicative? f)
         ; if f is a delayed-pure instance, check if any of the args are concrete instances to infer
         ; the real value of pure from them
         (let* ([concrete-instance (if (delayed-pure? f)
                                       (or (findf (negate delayed-pure?) args) f)
                                       f)]
                [coerce-concrete (coerce-pure concrete-instance)])
           (apply (coerce-concrete f) (map coerce-concrete args)))]
        [else (c:apply f args)]))

(define-syntax #%app-applicative
  (syntax-parser
    [(_ f:expr arg:expr ...) #'(apply-applicative f (list arg ...))]
    [(_ . rest)              #'(#%app . rest)]))

(struct delayed-pure (value)
  #:transparent
  #:reflection-name 'pure
  #:methods gen:functor
  [(define (map f x)
     (delayed-pure (f (delayed-pure-value x))))]
  #:methods gen:applicative
  [(define/generic -apply apply)
   (define (pure _ x) (delayed-pure x))
   (define (apply f args)
     (delayed-pure (-apply (delayed-pure-value f) (map delayed-pure-value args))))]
  #:methods gen:monad
  [(define (chain f x)
     (f (delayed-pure-value x)))])

(define/subexpression-pos-prop (pure/c ctc)
  (let ([ctc (coerce-contract 'pure/c ctc)])
    (rename-contract
     (struct/c delayed-pure ctc)
     (build-compound-type-name 'pure/c ctc))))
