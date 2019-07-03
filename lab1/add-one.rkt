#lang racket
(define add-one
  (lambda (x)
    (+ x 1)))

(define a 5)
(define b 6)
(define c 7)
(define strange
  (lambda (x)
    (let ((a 1) (b 2))
      (+ x a b))))

