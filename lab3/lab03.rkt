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

(define 1-set-equal?
  (lambda (L1 L2)
    (and (equal? (length L1) (length L2)) (subset? L1 L2)))) ;;if L1 and L2 are same length, and L1 is subset of L2

(define set-exists?
  (lambda (L1 L2)
    ;;L1 is 1 set containing no other sets
    ;;L2 is 1 set that could contain other sets
    ;;check if (null? L1)
        ;;false - check if (car L2) is a list
            ;;true - check if L1 subset (car L2)
                ;;true - return true
                ;;false - (set-exists? L1 (cdr L2))
            ;;false - set-exsits? L1 (cdr L2))
        ;;true - return true
    (if (null? L1) #t
        (if (null? L2) #f
            (if (list? (car L2))
                (if (1-set-equal? L1 (car L2)) #t
                    (set-exists? L1 (cdr L2)))
                (set-exists? L1 (cdr L2)))))))
                
            
(define set-equal?
  (lambda (L1 L2)
    ;;check if (null? L1)
        ;;false - check if (car L1) is a list
            ;;true - check if that list exists in L2
                ;;true - set-equal? (cdr L1 L2)
                ;;false - terminate
            ;;false - check if that x is a member of L2
                ;;true - set-equal? (cdr L1 L2)
                ;;false - terminate
        ;;true - return true
    (if (null? L1) #t
        (if (list? (car L1))
            (if (set-exists? (car L1) L2)
                (set-equal? (cdr L1) L2)
                #f)
            (if (member? (car L1) L2)
                (set-equal? (cdr L1) L2)
                #f)))))

(define union
  (lambda (S1 S2)
    ;;check if set-equal?
        ;;true - S1
        ;;false - check if (null? S1)
            ;;true - S2
            ;;false - check if (car S1) is a list
                ;;true - check if set-equal? (car S1) S2
                    ;;true - union (cdr S1) S2
                    ;;false - cons (car S1) union (cdr S1) S2)
                ;;false - check if (car S1) is a member of S2
                    ;;true - union (cdr S1) S2
                    ;;false - cons (car S1) (union (cdr S1) S2)
    (if (set-equal? S1 S2) S1
        (if (null? S1) S2
            (if (list? (car S1))
                (if (set-equal? (car S1) S2)
                    (union (cdr S1) S2)
                    (cons (car S1) (union (cdr S1) S2)))
                (if (member? (car S1) S2)
                    (union (cdr S1) S2)
                    (cons (car S1) (union (cdr S1) S2)))))))
                    
                    

            
  