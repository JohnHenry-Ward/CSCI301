#lang racket
;;reflexive? xRx
;;first checks if the relation is valid over the set S
;;if true, then it calls real-reflexive? (which is recursive)

(define reflexive?
  (lambda (L S)
    (if (not (relation? L S)) #f
        (real-reflexive? L S))))

(define real-reflexive?
  (lambda (L S)
    (if (null? S) #t
        (if (< (length L) (length S)) #f
            (if (equal? (car (car L)) (car (cdr (car L))))
                (real-reflexive? (cdr L) (remove (car (car L)) S)) ;;removes first position in S to make progress
                (real-reflexive? (cdr L) S))))))

;;-----end reflexive?-----;;        

;;symmetric? xRy => yRx
;;i am assuming that their could be duplicate pairs, since it's not specified in the lab

(define symmetric?
  (lambda (L)
    (if (null? L) #t
        (if (set-exists? (reverse (car L)) L)
            (if (equal? (car (car L)) (car (cdr (car L))))
                (symmetric? (remove* (list (car L)) L)) ;;removes only the pair that is reflexive (a, a)
                (symmetric? (remove* (list (reverse (car L))) (remove* (list (car L)) L)))) ;;removes both the initial pair and the reverse pair
            #f))))

;;-----end symmetric?-----;;

;;transitive? xRy and yRz => xRz
;;transitive? calls real-transitive? so that I can have 2 lists to use

(define transitive?
  (lambda (L)
    (real-transitive? L L)))

;;real-transitive? takes 2 lists, one is always the inital list L, the other is cdr L so that progress can be made

(define real-transitive?
  (lambda (L changeL)
    (if (null? changeL) #t
        (if (check-for-transitive L (car changeL) L)
            (real-transitive? L (cdr changeL)) ;;progress is made
            #f))))

;;check for transitive takes 3 inputs (this is where most of the work is done)
;;the inital list L, which is used to check if a new transitive pair is in L
;;a pair that is used to check for a possible new transitive pair
;;resetL which is used to go down L to check for possible transitive pair, and is 'reset' once all possible pairs have been found

(define check-for-transitive
 (lambda (L pair resetL)
   (if (null? resetL) #t
       (if (equal? (car (car resetL)) (car (cdr pair))) ;;if link is found (a b) (b c)
           (if (set-exists? (list (car pair) (car (cdr (car resetL)))) L) ;;check if new pair (a c) is in L
               (check-for-transitive L pair (cdr resetL)) ;;keep checking resetL for possible links
               #f)
           (check-for-transitive L pair (cdr resetL))))))

;;-----end transitive?-----;;
        

;;-----Helper Functions-----;;

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