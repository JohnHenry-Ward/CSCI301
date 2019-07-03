#lang racket
(define mystery
  (lambda (L)
    (if (null? L)
        L
        (begin
          (displayln L)
          (append (mystery (cdr L))
                (list (car L)))))))

