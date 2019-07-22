#lang racket
;;reflexive?: xRx for every x
(define reflexive?
  (lambda (L S)
    (if (null? S) #t
        (if (null? L) #f
            (if (and (equal? (car S) (car (car L))) (equal? (car S) (car (cdr (car L)))))
                (reflexive? L (cdr S))
                (if (null? L) #f
                    (reflexive? (cdr L) S)))))))
;;this one might not be done, waiting to see what yudong says


;;symmetric?: xRy => yRx
(define symmetric?
  (lambda (L)
    ;;if car L switched is in L
        ;;true, symmetric? (cdr L)
        ;;false, #f



;;transitive: xRy and yRz => xRz









(define subset?
  (lambda (L1 L2)
    (if (null? L1) ;;if L1 is empty, then it is a subset (empty set is always a subset)
        #t
        (if (member? (car L1) L2)
            (subset? (cdr L1) L2)
            #f))))