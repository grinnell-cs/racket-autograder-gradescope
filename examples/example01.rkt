#lang racket
(require rackunit)

(define increment
  (lambda (x)
    (+ 2 x)))

(test-true "One" #t)
(test-true "Two" #t)
(test-true "Three" #t)