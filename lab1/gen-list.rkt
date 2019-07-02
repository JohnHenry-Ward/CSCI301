#lang racket
(define gen '())
(define gen-list
  (lambda (start end)
    (if (> start end)
        '()
        (begin
        (append '(start) '(gen))
        (gen-list (+ 1 start) end)
        (displayln gen)))))
