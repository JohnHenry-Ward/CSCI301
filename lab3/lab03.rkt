#lang racket
;;-----helper functions-----;;

;;function checks if a single element is in a list L
(define member?
  (lambda (x L)
    (if (null? L) 
        #f
        (or (equal? x (car L)) (member? x (cdr L)))))) ;;first checks if 1st pos = x, then recursive call on cdr of list

;;function checks if a single set L1 a set within the set L2
;;L1 does not contain other sets, only single elements
(define set-exists?
  (lambda (L1 L2)
    (if (null? L1) #t
        (if (null? L2) #f
            (if (list? (car L2))
                (if (set-equal? L1 (car L2)) #t
                    (set-exists? L1 (cdr L2)))
                (set-exists? L1 (cdr L2)))))))

;;-----end helper functions-----;;
                
;;function checks if two sets are equal to each other (sets may contain sets)         
(define set-equal?
  (lambda (L1 L2)
    (if (null? L1) #t
        (if (list? (car L1))
            (if (set-exists? (car L1) L2)
                (set-equal? (cdr L1) L2)
                #f)
            (if (member? (car L1) L2)
                (set-equal? (cdr L1) L2)
                #f)))))

;;function returns a new set from all elements of two sets (ignoring duplicates)
(define union
  (lambda (S1 S2)
    (if (set-equal? S1 S2) S2
        (if (null? S1) S2
            ;;checks if its a list (longer than 1) or a single value
            (if (and (list? (car S1)) (> (length (car S1)) 1))
                ;;(car S1) is a list
                (if (set-equal? (car S1) S2)
                    (union (cdr S1) S2)
                    (cons (car S1) (union (cdr S1) S2)))
                ;;(car S1) is a single element, or a list with 1 item
                (if (member? (car S1) S2)
                    (union (cdr S1) S2)
                    (cons (car S1) (union (cdr S1) S2))))))))

;;function returns a new set from all elements that are in both sets (ignoring duplicates)
(define intersect
  (lambda (S1 S2)
    (if (or (null? S1) (null? S2)) '()
        (if (set-equal? S1 S2) S1
            ;;checks if its a list (longer than 1) or a single value
            (if (and (list? (car S1)) (> (length (car S1)) 1))
                ;;(car S1) is a list
                (if (set-equal? (car S1) S2)
                    (cons (car S1) (intersect (cdr S1) S2))
                    (intersect (cdr S1) S2))
                ;;(car S1) is a single element, or a list with 1 item
                (if (member? (car S1) S2)
                    (cons (car S1) (intersect (cdr S1) S2))
                    (intersect (cdr S1) S2)))))))
                    
                    

            
  