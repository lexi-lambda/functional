#lang racket/base

(require racket/require
         data/collection
         (multi-in racket [contract generic lazy-require match])
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(lazy-require ["applicative.rkt" (pure)]
              [(submod "applicative.rkt" coerce-delayed) (coerce-pure)])

(provide gen:monad monad?
         do <- (rename-out [chain-monad chain] [<- ←])
         (contract-out [join (monad? . -> . monad?)]
                       [map/m ((any/c . -> . monad?) sequence? . -> . monad?)]))

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
    [(_ [x:id (~and arrow <-) mx:expr] . rest)
     (with-disappeared-uses
      (begin
        (record-disappeared-uses (list #'arrow))
        #'(chain-monad (λ (x) (do . rest)) mx)))]
    [(_ (define . definition) ...+ . rest)
     #'(let () (define . definition) ... (do . rest))]
    [(_ mx:expr . rest)
     #'(chain-monad (λ (_) (do . rest)) mx)]))

(define (join x)
  (chain-monad values x))

(define/match (map/m f xs)
  [(f (sequence x xs ...))
   (do [y <- (f x)]
       [ys <- (map/m f xs)]
       (pure (cons y ys)))]
  [(_ _) (pure '())])
