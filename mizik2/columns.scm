#!/usr/bin/guile
!#
(use-modules (srfi srfi-1) (srfi srfi-9))

(define-record-type <mizik-column>
  (make-mizik-column columnTitle columnFormatter widthPercentage isSorted)
  mizik-column?
  (columnTitle     title          title-set!)
  (columnFormatter formatter      formatter-set!)
  (widthPercentage percentage     percentage-set!)
  (isSorted        column-sorted? column-sorted-set!))

(define (generate-columns protocolumns)
  (define total (fold
                  (lambda (protocolumn result)
                    (+ (string-length (car protocolumn)) result))
                  0
                  protocolumns))

  (map
    (lambda (protocolumn)
      (let ([titleWidth (car protocolumn)])
        (make-mizik-column titleWidth                           (cdr protocolumn)
                           (/ (string-length titleWidth) total) -1)))
    protocolumns))
