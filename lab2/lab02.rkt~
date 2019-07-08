#lang racket

(define member?
  (lambda (x L)
    (if (null? L) 
        #f
        (or (equal? x (car L)) (member? x (cdr L)))))) ;;first checks if 1st pos = x, then recursive call on cdr of list

(define subset?
  (lambda (L1 L2)
    ;;check if 1st of L1 is 1st L2
        ;;false - check if 1st of L1 is n+1 of L2
            ;;false - check if 1st of L1 is n+1 of L2
            ;;true - check if 2nd of L1 is n+1 of L2
        ;;true - check if 2nd of L1 is 1st of L2
            ;;false - check if 2nd of L1 is n+1 of L2
            ;;true - check if 3rd of L1 is 1st of L2
    (if(equal? (car L1) (car L2))
       (subset? (cdr L1) L2)
       (subset? (car L1) (cdr L2)))))