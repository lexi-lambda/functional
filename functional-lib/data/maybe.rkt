#lang curly-fn racket/base

(require racket/require
         (prefix-in c: data/collection)
         (multi-in data [functor applicative monad])
         (multi-in racket [contract function generic match serialize])
         (for-syntax racket/base
                     syntax/parse))

(provide maybe? just just? nothing nothing? maybe/c
         with-maybe-handler
         (contract-out
          [maybe (any/c (any/c . -> . any/c) maybe? . -> . any/c)]
          [from-just (any/c maybe? . -> . any/c)]
          [from-just! (just? . -> . any/c)]
          [filter-just ((listof maybe?) . -> . list?)]
          [map-maybe ((any/c . -> . maybe?) list? . -> . list?)]
          [false->maybe (any/c . -> . maybe?)]
          [exn->maybe ([(any/c . -> . any/c) procedure?] #:rest any/c . ->* . any/c)]))

(define (maybe? x)
  (or (just? x) (nothing? x)))

(serializable-struct just (value)
  #:transparent
  #:methods gen:functor
  [(define (map f x)
     (just (f (just-value x))))]
  #:methods gen:applicative
  [(define (pure _ x) (just x))
   (define/contract (apply f args)
     (any/c (listof maybe?) . -> . any/c)
     (if (andmap just? args)
         (just (c:apply (just-value f) (map just-value args)))
         nothing))]
  #:methods gen:monad
  [(define (chain f x)
     (f (just-value x)))])

(define nothing-value
  (let ()
    (struct nothing ()
      #:property prop:serializable
      (make-serialize-info (λ _ #())
                           #`deserialize-info:nothing-v0
                           #f
                           (or (current-load-relative-directory)
                               (current-directory)))
      #:methods gen:custom-write
      [(define (write-proc x out mode)
         (display "#<nothing>" out))]
      #:methods gen:functor
      [(define (map f x) nothing-value)]
      #:methods gen:applicative
      [(define (pure _ x) (just x))
       (define (apply f args)
         nothing-value)]
      #:methods gen:monad
      [(define (chain f x) nothing-value)])
    (define nothing-value (nothing))
    nothing-value))

(define deserialize-info:nothing-v0
  (make-deserialize-info (λ _ nothing-value)
                         (λ _ (error '|nothing: can't have cycles|))))
(module+ deserialize-info
  (provide deserialize-info:nothing-v0))

(define-match-expander nothing
  (syntax-parser [(_)        #'(== nothing-value)])
  (syntax-parser [(_ . args) #'(nothing-value . args)]
                 [_          #'nothing-value]))

(define nothing? #{eq? nothing})

(define/subexpression-pos-prop (maybe/c val/c)
  (let ([val/c (coerce-contract 'maybe/c val/c)])
    (rename-contract
     (or/c nothing? (struct/c just val/c))
     (build-compound-type-name 'maybe/c val/c))))

(define/match (maybe x f m)
  [(_ f (just x))  (f x)]
  [(x _ (nothing)) x])

(define/match (from-just x m)
  [(_ (just x))  x]
  [(x (nothing)) x])

(define (from-just! x)
  (just-value x))

(define/match (filter-just lst)
  [('()) '()]
  [((cons (just x) rest)) (cons x (filter-just rest))]
  [((cons (nothing) rest)) (filter-just rest)])

(define/match (map-maybe f lst)
  [(_ '()) '()]
  [(f (cons x rest))
   (match (f x)
     [(just x) (cons x (map-maybe f rest))]
     [(nothing) (map-maybe f rest)])])

(define (false->maybe x)
  (if x (just x) nothing))

(define-syntax-rule (with-maybe-handler pred? body ...)
  (with-handlers ([pred? (λ (_) nothing)])
    (just (let () body ...))))

(define (exn->maybe pred? proc . args)
  (with-maybe-handler pred? (apply proc args)))
