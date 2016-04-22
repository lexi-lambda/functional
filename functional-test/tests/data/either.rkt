#lang racket/base

(require racket/require
         (multi-in data [functor applicative monad either])
         rackunit
         rackunit/spec)

(describe "either"
  (describe "map"
    (it "maps over the internal value for right"
      (check-equal? (map add1 (right 12)) (right 13)))

    (it "is the identity for left"
      (check-equal? (map add1 (left 12)) (left 12))))

  (describe "pure"
    (it "wraps pure values in right"
      (check-equal? ((right values) (pure 'hello)) (right 'hello))))

  (describe "apply"
    (it "applies functions to values wrapped in just"
      (check-equal? ((right +) (right 1) (right 2)) (right 3)))

    (it "returns the first left if any of the values are left"
      (check-equal? ((left +) (left 1) (pure 2)) (left +))
      (check-equal? ((right +) (left 1) (left 2)) (left 1))
      (check-equal? ((right +) (right 1) (left 2)) (left 2))))

  (describe "chain"
    (it "threads a right value through the computation"
      (check-equal? (do [x <- (right 3)]
                        [y <- (right (* x 2))]
                        [z <- (right (sub1 y))]
                        (right (/ z 3)))
                    (right 5/3)))

    (it "aborts if any value returns left"
      (check-equal? (do [x <- (right 3)]
                        [y <- (left 'die)]
                        [z <- (right (sub1 y))]
                        (right (/ z 3)))
                    (left 'die)))))

(describe "either"
  (it "applies a function to a right value"
    (check-equal? (either #f add1 (right 2)) 3))

  (it "returns a default for a left value"
    (check-equal? (either #f add1 (left 'fail)) #f)))

(describe "from-either"
  (it "returns a value inside of a just"
    (check-equal? (from-either #f (right 3)) 3))

  (it "returns a default for a left value"
    (check-equal? (from-either #f (left 'fail)) #f)))
