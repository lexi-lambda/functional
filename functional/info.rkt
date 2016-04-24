#lang info

(define collection 'multi)

(define deps
  '("base"
    "functional-lib"
    "functional-doc"))
(define build-deps
  '())

(define implies
  '("functional-lib"
    "functional-doc"))