#!/usr/local/bin/guile
!#

(define-syntax if-let
  (syntax-rules ()
    [(_ ([binding value] ...) then else) (let ([binding value] ...)
                                           (if (and binding ...) then else))]))
(define-syntax return-if
  (syntax-rules ()
    [(_ ret else) (if ret ret else)]))

(define-syntax case-pred
  (syntax-rules (else)
    [(_ key [pred result] ... [else lastResort]) (cond
                                                  [(pred key)     result]
                                                  ...
                                                  [else       lastResort])]
    [(_ key [pred result] ...)                   (cond
                                                  [(pred key) result] ...)]))

(define (2+ num)
  (+ num 2))
(define (2- num)
  (- num 2))

(define (caddddr lst)
  (car (cddddr lst)))

(define (alist . xs)
  (if (odd? (length xs))
      (error "ERROR: In procedure alist: uneven amount of arguments")
    (reverse (fold
               (lambda (elem previous)
                 (if (or (null? previous) (not (list? (car previous))))
                     (cons (list elem) previous)
                   (cons (cons (car (car previous)) elem) (cdr previous))))
               '()
               xs))))

(define (pos-or-to-zero num)
  (if (negative? num) 0 num))

(define (between? num1 num2 num3)
  (let ([lesser  (if (< num1 num3) num1 num3)]
        [greater (if (> num1 num3) num1 num3)])
    (and (> num2 lesser) (< num2 greater))))

(define* (: receivedAtom sI #:optional [eI (length receivedAtom)])
  (let* ([loRA       (length receivedAtom)]
         [startIndex (if (negative? sI)
                         (if (negative? (+ loRA sI)) 0 (+ loRA sI))
                       sI)]
         [endIndex   (if (negative? eI)
                         (if (> (+ loRA eI) loRA) loRA (+ loRA eI))
                       (if (> eI loRA) loRA eI))])
    (if (> startIndex endIndex)
        '()
      (let* ([listHead      (list-head receivedAtom endIndex)]
             [lenOfListHead                 (length listHead)])
        (list-tail listHead (if (> startIndex lenOfListHead)
                                lenOfListHead
                              startIndex))))))
