#lang racket
(define gen-list
  (lambda (start end)
    (if (> start end)
        '()
        (cons start (gen-list (+ 1 start) end)))))

(define sum
  (lambda (lst)
    (if (null? lst)
        lst
        (+ (car lst) (sum (cdr lst))))))


          

       
    