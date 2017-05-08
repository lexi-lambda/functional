#lang curly-fn racket/base

(require racket/require
         racket/serialize
         (multi-in data [functor applicative monad maybe])
         racket/match
         rackunit
         rackunit/spec)

(describe "maybe"
  (it "is serializable"
    (check-eq? (deserialize (serialize nothing)) nothing)
    (check-equal? (deserialize (serialize (just 3))) (just 3)))

  (describe "map"
    (it "maps over the internal value for just"
      (check-equal? (map add1 (just 12)) (just 13)))

    (it "returns nothing for nothing"
      (check-equal? (map add1 nothing) nothing)))

  (describe "pure"
    (it "wraps pure values in just"
      (check-equal? ((just values) (pure 'hello)) (just 'hello))))

  (describe "apply"
    (it "applies functions to values wrapped in just"
      (check-equal? ((just +) (just 1) (just 2)) (just 3)))

    (it "returns nothing if the arguments are nothing"
      (check-equal? ((just +) nothing (just 2)) nothing))
    
    (it "returns nothing if the procedure is nothing"
      (check-equal? (nothing (pure 1) (just 2)) nothing)))

  (describe "chain"
    (it "threads a just value through the computation"
      (check-equal? (do [x <- (just 3)]
                        [y <- (just (* x 2))]
                        [z <- (just (sub1 y))]
                        (just (/ z 3)))
                    (just 5/3)))

    (it "aborts if any value returns nothing"
      (check-equal? (do [x <- (just 3)]
                        [y <- nothing]
                        [z <- (just (sub1 y))]
                        (just (/ z 3)))
                    nothing)))

  (describe "nothing"
    (it "prints like an opaque value"
      (check-equal? (format "~a" nothing) "#<nothing>"))

    (it "functions as a match expander"
      (check-equal? (match (just 3) [(just _) #t] [(nothing) #f]) #t)
      (check-equal? (match nothing  [(just _) #t] [(nothing) #f]) #f))))

(describe "maybe"
  (it "applies a function to a just value"
    (check-equal? (maybe #f add1 (just 2)) 3))

  (it "returns a default for nothing"
    (check-equal? (maybe #f add1 nothing) #f)))

(describe "from-just"
  (it "returns a value inside of a just"
    (check-equal? (from-just #f (just 3)) 3))

  (it "returns a default for nothing"
    (check-equal? (from-just #f nothing) #f)))

(describe "false->maybe"
  (it "returns nothing for #f"
    (check-equal? (false->maybe #f) nothing))

  (it "returns just for non-#f values"
    (check-equal? (false->maybe 'hello) (just 'hello))))

(describe "with-maybe-handler"
  (it "converts exceptions to nothing"
    (check-equal? (with-maybe-handler exn:fail?
                    (bytes->string/utf-8 #"\xC3"))
                  nothing))

  (it "wraps successful computations in just"
    (check-equal? (with-maybe-handler exn:fail?
                    (bytes->string/utf-8 #"hello"))
                  (just "hello"))))

(describe "exn->maybe"
  (define try-bytes->string/utf-8
    #{exn->maybe exn:fail? bytes->string/utf-8})
  
  (it "converts exceptions to nothing"
    (check-equal? (try-bytes->string/utf-8 #"\xC3") nothing))

  (it "wraps successful computations in just"
    (check-equal? (try-bytes->string/utf-8 #"hello") (just "hello"))))

(check-pred nothing?
            (deserialize (serialize (nothing)))
            "nothing deserializes to itself")

(check-equal? (just 3)
              (deserialize (serialize (just 3)))
              "just is serializable")