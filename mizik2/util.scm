#!/usr/bin/guile
!#

(define ELLIPSIS              "…")
(define  PLAY_WINDOW_HEIGHT     3)
(define  PLAY_STATUS_ICON   " ▶ ")
(define PAUSE_STATUS_ICON   " 𝍪 ")
(define  STOP_STATUS_ICON   " ▪ ")

(define-syntax if-let-helper
  (syntax-rules ()
    [(_ letVersion
        ([bnd             val]    ...)
        (cnd                      ...)
        ()                             then else) (letVersion ([bnd val] ...)
                                                    (if (and cnd ...) then else))]
    [(_ letVersion
        ([bnd             val]    ...)
        (cnd                      ...)
        ([binding       value] . rest) then else) (if-let-helper letVersion
                                                                 ([bnd val] ... [binding value])
                                                                 (cnd       ...           value)
                                                                 rest                            then else)]
    [(_ letVersion
        ([bnd             val]    ...)
        (cnd                      ...)
        ([binding funct value] . rest) then else) (if-let-helper letVersion
                                                                 ([bnd val] ... [binding value])
                                                                 (cnd       ... (funct binding))
                                                                 rest                            then else)]))
(define-syntax if-let
  (syntax-rules ()
    [(_ ([binding         value]  ...) then else) (let ([binding value] ...)
                                                    (if (and binding ...) then else))]
    [(_ (binding-funct-value      ...) then else) (if-let-helper let
                                                                 ()
                                                                 ()
                                                                 (binding-funct-value ...) then else)]))
(define-syntax if-let*
  (syntax-rules ()
    [(_ ([binding         value]  ...) then else) (let* ([binding value] ...)
                                                    (if (and binding ...) then else))]
    [(_ (binding-funct-value      ...) then else) (if-let-helper let*
                                                                 ()
                                                                 ()
                                                                 (binding-funct-value ...) then else)]))
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
