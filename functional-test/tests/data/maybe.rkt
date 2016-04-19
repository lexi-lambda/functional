#lang racket/base

(require racket/require
         (multi-in data [functor applicative monad maybe])
         rackunit
         rackunit/spec)

(describe "maybe"
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
      (check-equal? (nothing (just 1) (just 2)) nothing)))

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
                    nothing))))
