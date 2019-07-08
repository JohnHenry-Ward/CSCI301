#lang racket

(define member?
  (lambda (x L)
    (if (null? L) 
        #f
        (or (equal? x (car L)) (member? x (cdr L)))))) ;;first checks if 1st pos = x, then recursive call on cdr of list

(define subset?
  (lambda (L1 L2)
    (if (null? L1) ;;if L1 is empty, then it is a subset (empty set is always a subset)
        #t
        (if (member? (car L1) L2)
            (subset? (cdr L1) L2)
            #f))))

(define set-equal?
  (lambda (L1 L2)
    (and (equal? (length L1) (length L2)) (subset? L1 L2)))) ;;if L1 and L2 are same length, and L1 is subset of L2

(define union
  (lambda (S1 S2)
    (if (null? S1) S2
        (if (member? (car S1) S2)
        (union (cdr S1) S2)
        (cons (car S1) (union (cdr S1) S2))))))
          
(define intersect
  (lambda (S1 S2)
    (if (or (null? S1) (null? S2)) '() 
        (if (set-equal? S1 S2) S1 
            (if (member? (car S1) S2)
                (cons (car S1) (intersect (cdr S1) S2))
                (intersect (cdr S1) S2))))))
                