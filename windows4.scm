#!/usr/local/bin/guile
!#
(use-modules (srfi srfi-1) (ice-9 threads) (ice-9 atomic))

(define (windows::build-columned-window win mpd . captions)
  (define (build-columns wind elements columnsOrCaptions playWinHt)
    (let ([windowWidth  (getmaxx wind)]
          [captionTotal (if elements -1 (fold
                                          (lambda (elem ret)
                                            (+ (string-length (car elem)) ret))
                                          0
                                          columnsOrCaptions))])
      (let loop ([result                '()]
                 [current columnsOrCaptions]
                 [offset                  0])
        (if (null? (cdr current))
            (reverse (cons (if elements
                               ((car current) #:rebuild-with-size
                                                elements
                                                offset
                                                (- windowWidth offset)
                                                playWinHt)
                             (let ([cc (car current)])
                               (column
                                 wind
                                 (cdr cc)
                                 (car cc)
                                 '()
                                 offset
                                 (cons #f (- windowWidth offset))))) result))
          (let* ([cc  (car current)]
                 [col (if elements
                          (cc #:rebuild-with-size elements offset #f playWinHt)
                        (let ([ccs (car cc)])
                          (column wind (cdr cc) ccs
                                  '()  offset   (cons
                                                  #t
                                                  (/ (string-length
                                                       ccs) captionTotal)))))])
            (loop
              (cons col result)
              (cdr current)
              (+ (col #:get-width) offset)))))))

  (define* (column window format-line headerSymbol
                   lines  offset      percentage   #:optional [formattedLines #f]
                                                              [setHeader      #f]
                                                              [setLines       #f])
    (define body        (if formattedLines
                            formattedLines
                          (map (lambda (line)
                                 (format-line
                                   (assoc-ref line headerSymbol))) lines)))
    (define columnWidth (if (car percentage)
                            (round (* (cdr percentage) (getmaxx window)))
                          (cdr percentage)))



    (define (check-height newLinesLength playWindowHeight)
      (let ([linesLength (1- (- (getmaxy window) playWindowHeight))])
        (when (> newLinesLength linesLength)
          (error (string-append
                   "In procedure column#:add-new-line: additional line "
                   "pushes lines length to larger value "
                   "(" (number->string newLinesLength) ") than window height "
                   "(" (number->string    linesLength) ")")))))
    (define (form-line-of-approp-len l)
      (let* ([line                (if l l "")]
             [ELLIPSIS                    "…"]
             [lineLength (string-length line)])
        (cond
         [(<= columnWidth 0)                            ""]
         [(=  columnWidth 1)                      ELLIPSIS]
         [(=  columnWidth 2)  (string-append ELLIPSIS " ")]
         [(>
            lineLength
            (1- columnWidth)) (string-append
                                (substring line 0 (2- columnWidth))
                                ELLIPSIS
                                " ")]
         [else                                        line])))
    (define (format-and-add line index)
      (let ([newLine (form-line-of-approp-len line)])
        (addstr window newLine #:y index #:x offset)

        newLine))



    (let ([newHeader (if setHeader
                         setHeader
                       (format-and-add (symbol->string headerSymbol) 0))]
          [newLines  (if setLines
                         setLines
                       (map format-and-add body (iota (length body) 1)))]
          [linesLen  (length lines)])
      (lambda (method . xs)
        (cond
         [(eq? method #:get-width)                                 columnWidth]
         [(eq? method #:get-tag)                                  headerSymbol]
         [(eq? method #:get-header)              (symbol->string headerSymbol)]
         [(eq? method #:get-formed-header)                           newHeader]
         [(eq? method #:get-lines)                                       lines]
         [(eq? method #:get-formatted)                                    body]
         [(eq? method #:get-formed-lines)                             newLines]
         [(eq? method #:form-line-of-approp-len)       form-line-of-approp-len]
         [(eq? method #:highlight-column)
               (for-each
                 (lambda (index)
                   (chgat window columnWidth (if (cadr xs)
                                                 A_REVERSE
                                               A_NORMAL)   0
                          #:x    offset      #:y           index))
                 (iota (- (getmaxy window) (car xs))))]
         [(eq? method #:add-new-line)
               (let* ([line                                          (car xs)]
                      [index   (if (> (cadr xs) linesLen) linesLen (cadr xs))]
                      [i+1                                         (1+ index)]
                      [newLen          (if (caddr xs) linesLen (1- linesLen))]
                      [refLine                  (assoc-ref line headerSymbol)])
                 (check-height newLen (cadddr xs))

                 (column
                   window
                   format-line
                   headerSymbol
                   (append (: lines 0 index) (list line) (: lines index newLen))
                   offset
                   percentage
                   (append (: body  0 index) (list
                                               refLine)  (: body  index newLen))
                   newHeader
                   (append
                     (: newLines 0 index)
                     (list (format-and-add (format-line refLine) (1+ index)))
                     (let ([sub (: body index newLen)])
                       (map
                         format-and-add
                         sub
                         (iota (length sub) (2+ index)))))))]
         [(eq? method #:rebuild)                                 (column
                                                                   window
                                                                   format-line
                                                                   headerSymbol
                                                                   (car xs)
                                                                   offset
                                                                   percentage)]
         [(eq? method #:rebuild-with-size)       (check-height
                                                   (length (car xs))
                                                   (cadddr xs))

                                                 (column
                                                   window
                                                   format-line
                                                   headerSymbol
                                                   (car  xs)
                                                   (cadr xs)
                                                   (let ([perc (caddr xs)])
                                                     (if perc
                                                         (cons #f perc)
                                                       percentage)))]
         [(eq? method #:rebuild-manually)
               (let ([perc (car xs)] [offSet (cadr xs)] [clrHt (caddr xs)])
                 (clear-lines window (if clrHt
                                         clrHt
                                       (1+ (length lines))) 0 offSet)

                 (column
                   window
                   format-line
                   headerSymbol
                   lines
                   offSet
                   (if (car percentage)
                       (cons #t (let ([p (+ (cdr percentage) perc)])
                                  (if (<= (+ p perc perc) 0)
                                      (cdr percentage)
                                    p)))
                     (cons #f (- (getmaxx window) offSet)))))]))))

  (define (columned-window window       playWindow mpdClient
                           masterList   allColumns isInSelectionMode
                           highlightPos begPos     endPos)
    )

  (define* (play-window window runningThread sBox dBox #:optional [height 3])
    )



  (columned-window
    win
    (play-window   win #f (make-atomic-box #f) (make-atomic-box #f))
    mpd
    '()
    (build-columns win #f captions             #f)
    (cons #f #f)
    0
    0
    0))
