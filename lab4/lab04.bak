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
    (if (null? L) #t
        (if (set-exists? (reverse (car L)) L)
            (if (not (equal? (car L) (reverse (car L))))
                (and (remove (reverse (car L)) L) (symmetric? (cdr L)))
                (symmetric? (cdr L)))
            #f))))
;;check if subsets of each other???



;;transitive: xRy and yRz => xRz




;;-----Helper Functions-----;;
        
;;function checks if a single set L1 a set within the set L2
;;L1 does not contain other sets, only single elements
(define set-exists?
  (lambda (L1 L2)
    (if (null? L1) #t
        (if (null? L2) #f
            (if (equal? L1 (car L2)) #t
                (set-exists? L1 (cdr L2)))))))