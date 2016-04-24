#lang racket/base

(require data/collection
         racket/contract
         racket/generic
         racket/lazy-require
         (for-syntax racket/base
                     syntax/parse))

(lazy-require ["applicative.rkt" (pure)]
              [(submod "applicative.rkt" coerce-delayed) (coerce-pure)])

(provide gen:monad monad?
         do <- (rename-out [chain-monad chain] [<- ←])
         join)

(define-generics monad
  (chain f monad)
  #:defaults
  ([sequence?
    (define/contract (chain f xs)
      ((any/c . -> . sequence?) any/c . -> . sequence?)
      (for*/sequence ([x (in xs)]
                      [y (in (f x))])
        y))]))

; wrap a binding function so that delayed-pure instances will be properly converted to
; instance-specific values using their pure implementation
; monad? (any/c -> monad?) -> any/c -> monad?
(define ((wrap-unpure x f) y)
  ((coerce-pure x) (f y)))

(define (chain-monad f x)
  (chain (wrap-unpure x f) x))

(define-syntax (<- stx)
  (raise-syntax-error '<- "cannot be used outside of a do block" stx))

(define-syntax do
  (syntax-parser
    #:literals [define <-]
    [(_ x:expr) #'x]
    [(_ [x:id <- mx:expr] . rest)
     #'(chain-monad (λ (x) (do . rest)) mx)]
    [(_ (define . definition) ...+ . rest)
     #'(let () (define . definition) ... (do . rest))]
    [(_ mx:expr . rest)
     #'(chain-monad (λ (_) (do . rest)) mx)]))

(define (join x)
  (chain-monad values x))
