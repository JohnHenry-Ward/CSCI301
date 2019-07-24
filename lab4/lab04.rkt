#lang racket
;;reflexive?: xRx for every x
(define reflexive?
  (lambda (L S)
    (if (null? S) #t
        (if (not (relation? L S)) #f
            (if (< (length L) (length S)) #f
                (if (equal? (car (car L)) (car (cdr (car L))))
                    (reflexive? (cdr L) (remove (car (car L)) S))
                    (reflexive? (cdr L) S)))))))


;;symmetric?: xRy => yRx
(define symmetric?
  (lambda (L)
    (if (null? L) #t
        (if (set-exists? (reverse (car L)) L)
            (if (equal? (car (car L)) (car (cdr (car L))))
                (symmetric? (remove* (list (car L)) L))
                (symmetric? (remove* (list (reverse (car L))) (remove* (list (car L)) L))))
            #f))))


;;transitive: xRy and yRz => xRz
;;(define transitive?
  ;;(lambda (L)
    ;;(if (null? L) #t
        


;;-----Helper Functions-----;;

;;function checks if their is a set (y z) in L when given pair (x y), and returns that pair (y z)
(define link-found?
  (lambda (L pair)
    (if (null? L) '()
        (if (equal? (car (cdr pair)) (car (car L)))
            (car L)
            (link-found? (cdr L) pair)))))

;;function checks if a set L is a relation of the set S
(define relation?
  (lambda (L S)
    (if (null? L) #t
        (if (and (member? (car (car L)) S) (member? (car (cdr (car L))) S))
            (relation? (cdr L) S)
            #f))))

;;function checks if a single element is in a list L
(define member?
  (lambda (x L)
    (if (null? L) 
        #f
        (or (equal? x (car L)) (member? x (cdr L)))))) ;;first checks if 1st pos = x, then recursive call on cdr of list
     
;;function checks if a single set L1 is a set within the set L2
;;L1 does not contain other sets, only single elements
(define set-exists?
  (lambda (L1 L2)
    (if (null? L1) #t
        (if (null? L2) #f
            (if (equal? L1 (car L2)) #t
                (set-exists? L1 (cdr L2)))))))