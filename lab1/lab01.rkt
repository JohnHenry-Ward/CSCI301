#lang racket
(define gen-list
  (lambda (start end)
    (if (> start end)
        '()
        (cons start (gen-list (+ 1 start) end)))))

(define sum
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum (cdr lst))))))

(define retrieve-first-n
  (lambda (n lst)
    (cond ((< n 1) '())
          ((> n (length lst)) '())
          (else (cons (car lst) (retrieve-first-n (- n 1) (cdr lst)))))))

          
(define pair-sum?
  (lambda (lst n)
    (if (null? lst) #f ;;if the list is empty
        (if (null? (cdr lst)) #f ;;if on the last element of the list (no pairs available)
            (if (= (+ (car lst) (car (cdr lst))) n)
                #t
                (pair-sum? (cdr lst) n))))))
       
    