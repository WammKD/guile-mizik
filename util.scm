#!/usr/local/bin/guile
!#

(define-syntax if-let
  (syntax-rules ()
    [(_ ([binding value] ...) then else) (let ([binding value] ...)
                                           (if (and binding ...) then else))]))

(define (2+ num)
  (+ num 2))
(define (2- num)
  (- num 2))

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
