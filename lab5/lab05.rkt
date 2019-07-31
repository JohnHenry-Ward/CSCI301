#lang racket
;;reflexive-closure
;;wrapper function that checks if initial L is reflexive, if so, no work needs to be done
(define reflexive-closure
  (lambda (L S)
    (if (reflexive? L S) L
        (real-reflexive-closure L L S S))))

;;recursively finds which pairs are missing from L, and leaves them in S, otherwise removes them
(define real-reflexive-closure
  (lambda (L fullL S fullS)
    (if (null? L) (print-reflexive-closure fullL S fullS)
        (if (equal? (car (car L)) (car (cdr (car L))))
            (real-reflexive-closure (cdr L) fullL (remove (car (car L)) S) fullS) ;;remove pair from L and S, since it's not missing
            (real-reflexive-closure (cdr L) fullL S fullS))))) ;;leave pair in S, since it is missing

;;called once all of L has been checked, and adds the 'missing' pairs into L
(define print-reflexive-closure
  (lambda (L missing S)
    (if (null? missing) ;;if we have added all missing pairs
        (if (reflexive? L S) L ;;not sure if this part is necessary but it checks if after missing components are added, then checks if L is truly reflexive
            (displayln (append L " but L is still not reflexive")))
        (print-reflexive-closure (append L (list (append (list (car missing)) (list (car missing))))) (cdr missing) S))))

;;-----end reflexive-closure-----;;

;;symmetric-closure
;;wrapper funciton that checks if initial L is symmetric, if so, no work needs to be done
(define symmetric-closure
  (lambda (L)
    (if (symmetric? L) L
        (real-symmetric-closure L L))))

;;function recursively checks if reverse of pair in L, L is used to make progress, fullL is only added to
(define real-symmetric-closure
  (lambda (L fullL)
    (if (null? L) fullL
        (if (set-exists? (reverse (car L)) L)
            (real-symmetric-closure (cdr L) fullL) ;;if symmetric pair is in L, nothing needs to be added
            (real-symmetric-closure (cdr L) (append (list (reverse (car L))) fullL)))))) ;;if symmetric pair NOT in L, add it to fullL

;;-----end symmetric-closure-----;;

;;transitive-closure

;;wrapper function checks if initial L is transitive, if so, no work needs to be done
(define transitive-closure
  (lambda (L)
    (if (transitive? L) L
        (real-transitive-closure L L))))

;;function recursively makes progress and adds the missing pairs to fullL
(define real-transitive-closure
  (lambda (L fullL)
    (if (null? L) fullL
        (if (check-for-transitive fullL (car L) fullL)
            (real-transitive-closure (cdr L) fullL)
            (real-transitive-closure (cdr L) (append (add-missing-pairs fullL (car L) fullL '()) fullL))))))

;;function finds missing pairs, adds them to a list, and returns them
(define add-missing-pairs
  (lambda (L pair resetL missingPairs)
    (if (null? resetL) missingPairs
        (if (equal? (car (car resetL)) (car (cdr pair))) ;;if link is found (a b) (b c)
            (if (set-exists? (list (car pair) (car (cdr (car resetL)))) L) ;;check if new pair (a c) is in L
                (add-missing-pairs L pair (cdr resetL) missingPairs) ;;keep checking resetL for possible links
                (add-missing-pairs L pair (cdr resetL) (append (list (list (car pair) (car (cdr (car resetL))))) missingPairs))) ;;keep checking resetL for possible links and add the missing pair to missingPairs
            (add-missing-pairs L pair (cdr resetL) missingPairs)))))

;;-----end transitive-closure-----;;
                                 
;;-----Helper Functions-----;;
        
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