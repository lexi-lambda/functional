#lang info

(define collection 'multi)

(define version "0.1")

(define deps
  '("base"
    "functional-lib"
    "functional-doc"))
(define build-deps
  '())

(define implies
  '("functional-lib"
    "functional-doc"))