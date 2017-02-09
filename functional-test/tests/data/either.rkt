#lang racket/base

(require racket/require
         racket/serialize
         (multi-in data [functor applicative monad maybe either])
         rackunit
         rackunit/spec)

(describe "either"
  (describe "map"
    (it "maps over the internal value for success"
      (check-equal? (map add1 (success 12)) (success 13)))

    (it "is the identity for failure"
      (check-equal? (map add1 (failure 12)) (failure 12))))

  (describe "pure"
    (it "wraps pure values in success"
      (check-equal? ((success values) (pure 'hello)) (success 'hello))))

  (describe "apply"
    (it "applies functions to values wrapped in just"
      (check-equal? ((success +) (success 1) (success 2)) (success 3)))

    (it "returns the first failure if any of the values are failure"
      (check-equal? ((failure +) (failure 1) (pure 2)) (failure +))
      (check-equal? ((success +) (failure 1) (failure 2)) (failure 1))
      (check-equal? ((success +) (success 1) (failure 2)) (failure 2))))

  (describe "chain"
    (it "threads a success value through the computation"
      (check-equal? (do [x <- (success 3)]
                        [y <- (success (* x 2))]
                        [z <- (success (sub1 y))]
                        (success (/ z 3)))
                    (success 5/3)))

    (it "aborts if any value returns failure"
      (check-equal? (do [x <- (success 3)]
                        [y <- (failure 'die)]
                        [z <- (success (sub1 y))]
                        (success (/ z 3)))
                    (failure 'die)))))

(describe "either"
  (it "applies a function to a success value"
    (check-equal? (either symbol->string add1 (success 2)) 3))

  (it "applies a function to a failure value"
    (check-equal? (either symbol->string add1 (failure 'fail)) "fail")))

(describe "from-success"
  (it "returns a value inside of a success"
    (check-equal? (from-success #f (success 3)) 3))

  (it "returns a default for a failure value"
    (check-equal? (from-success #f (failure 'fail)) #f)))

(describe "from-failure"
  (it "returns a value inside of a failure"
    (check-equal? (from-failure #f (failure 'fail)) 'fail))

  (it "returns a default for a success value"
    (check-equal? (from-failure #f (success 3)) #f)))

(describe "from-either"
  (it "returns a value from inside of a success"
    (check-equal? (from-either (success 3)) 3))

  (it "returns a value inside of a failure"
    (check-equal? (from-either (failure 'fail)) 'fail)))

(describe "map-failure"
  (it "maps over failure values"
    (check-equal? (map-failure add1 (failure 1)) (failure 2)))

  (it "leaves success values unchanged"
    (check-equal? (map-failure add1 (success 1)) (success 1))))

(describe "either->maybe"
  (it "converts success values to just values"
    (check-equal? (either->maybe (success 'x)) (just 'x)))

  (it "converts failure values to nothing"
    (check-equal? (either->maybe (failure 'x)) nothing)))

(describe "maybe->either"
  (it "converts just values to success values"
    (check-equal? (maybe->either 'a (just 'b)) (success 'b)))

  (it "creates a failure value from a default given nothing"
    (check-equal? (maybe->either 'x nothing) (failure 'x))))

(describe "flip-either"
  (it "converts success values to failure values"
    (check-equal? (flip-either (success 'x)) (failure 'x)))

  (it "converts failure values to success values"
    (check-equal? (flip-either (failure 'x)) (success 'x))))

(check-equal? (success 12)
              (deserialize (serialize (success 12)))
              "success is serializable")

(check-equal? (failure 12)
              (deserialize (serialize (failure 12)))
              "failure is serializable")
