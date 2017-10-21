#!/usr/local/bin/guile
!#
(use-modules (ncurses curses)
             (srfi    srfi-1))

(define (2+ num)
  (+ num 2))
(define (2- num)
  (- num 2))
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

(define (windows::build-columned-window win . captions)
  (define (build-columns wind elements columnsOrCaptions)
    (let ([windowWidth  (getmaxx wind)]
          [captionTotal (if elements
                            -1
                          (fold
                            (lambda (elem ret)
                              (+ (* (string-length elem) 1.0) ret))
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
                                                (- windowWidth offset))
                             (column
                               wind
                               (list (car current))
                               offset
                               (- windowWidth offset))) result))
          (let* ([cc  (car current)]
                 [col (if elements
                          (cc #:rebuild-with-size elements offset #f)
                        (column
                          wind
                          (list cc)
                          offset
                          (/ (string-length cc) captionTotal)))])
            (loop
              (cons col result)
              (cdr current)
              (+ (col #:get-width) offset)))))))

  (define* (column window lines offset percentage #:optional
                                                    [refinedLines #f]
                                                    [formatted    #f])
    (define header      (car lines))
    (define body        (if refinedLines
                            (cdr refinedLines)
                          (map
                            (lambda (line)
                              (assoc-ref line (string->symbol header)))
                            (cdr lines))))
    (define columnWidth (if (exact? percentage)
                            percentage
                          (inexact->exact
                            (round (* percentage (getmaxx window))))))

    (define (check-height newLinesLength)
      (let ([windowLength (getmaxy window)])
        (when (> newLinesLength windowLength)
          (error (string-append
                   "In procedure column#:get-new-line: additional line "
                   "pushes lines length to larger value "
                   "(" (number->string newLinesLength) ") than window height "
                   "(" (number->string windowLength)   ")")))))
    (define (calc-new-line l)
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
         [else                (string-concatenate
                                (cons line (make-list
                                             (- columnWidth lineLength)
                                             " ")))])))
    (define (format-and-add line index)
      (let ([newLine (calc-new-line line)])
        (addstr window newLine #:y index #:x offset)

        newLine))

    (let ([newLines (if formatted
                        formatted
                      (cons
                        (let ([newLine (calc-new-line header)])
                          (addstr window newLine #:y 0 #:x offset)
                          newLine)
                        (map format-and-add body (iota (length body) 1))))]
          [linesLen (1+ (length body))])
      (lambda (method . xs)
        (cond
         [(eq? method #:get-width)                             columnWidth]
         [(eq? method #:get-tag)                   (string->symbol header)]
         [(eq? method #:get-header)                                 header]
         [(eq? method #:get-formed-header)                  (car newLines)]
         [(eq? method #:get-lines)                             (cdr lines)]
         [(eq? method #:get-refined)                                  body]
         [(eq? method #:calc-new-line)                       calc-new-line]
         [(eq? method #:add-new-line)      (let* ([newLen  (1+
                                                             (length lines))]
                                                  [line             (car xs)]
                                                  [refLine (assoc-ref
                                                             line
                                                             (string->symbol
                                                               header))])
                                             (check-height newLen)

                                             (column
                                               window
                                               (append lines (list line))
                                               offset
                                               percentage
                                               (append body (list refLine))
                                               (append
                                                 newLines
                                                 (list (format-and-add
                                                         refLine
                                                         (1- newLen))))))]
         [(eq? method #:rebuild)           (column
                                             window
                                             (cons header (car xs))
                                             offset
                                             percentage)]
         [(eq? method #:rebuild-with-size) (check-height
                                             (length (car xs)))

                                           (column
                                             window
                                             (cons header (car xs))
                                             (cadr xs)
                                             (let ([perc (caddr xs)])
                                               (if perc
                                                   perc
                                                 percentage)))]))))

  (define (columned-window window       masterList allColumns
                           highlightPos begPos     endPos)
    (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
    (if (>= highlightPos (getmaxy window))
        (error highlightPos)
      (chgat window -1 A_REVERSE 0 #:x 0 #:y highlightPos))

    (lambda (method . xs)
      (cond
       [(eq? method #:get-window)              window]
       [(eq? method #:get-max-y-x)  (getmaxyx window)]
       [(eq? method #:get-max-y)    (getmaxy  window)]
       [(eq? method #:get-max-x)    (getmaxx  window)]
       [(eq? method #:refresh)      (refresh  window)]
       [(eq? method #:move-cursor)
             (let ([newPos               (+ highlightPos (car xs))]
                   [winLen                        (getmaxy window)]
                   [listLen                    (length masterList)]
                   [lastVisibleLineOfWin         (- endPos begPos)])
               (when (not (= highlightPos 0))
                 (chgat window -1 A_NORMAL 0 #:x 0 #:y highlightPos))

               (cond
                [(between? 0 newPos (1+ lastVisibleLineOfWin))
                      (columned-window
                        window
                        masterList
                        allColumns
                        (if (and (negative? newPos) (zero? highlightPos))
                            0
                          (if (< newPos 1) 1 (if (< listLen newPos)
                                                 (1- listLen)
                                               newPos)))
                        begPos
                        endPos)]
                [(or
                   (and
                     (> newPos lastVisibleLineOfWin)
                     (= highlightPos lastVisibleLineOfWin)
                     (= endPos listLen))
                   (and
                     (< newPos                    1)
                     (= highlightPos                    1)
                     (= begPos 0))
                   (and
                     (< newPos                    0)
                     (= highlightPos                    0)))
                      (columned-window window       masterList allColumns
                                       highlightPos begPos     endPos)]
                [(or (< newPos 1) (> newPos lastVisibleLineOfWin))
                      (let ([newBegPos ((if (< newPos 1) 1- 1+) begPos)]
                            [newEndPos (if (and
                                             (< newPos highlightPos)
                                             (not (=
                                                    (- endPos begPos)
                                                    (1- winLen))))
                                           endPos
                                         ((if (< newPos 1) 1- 1+) endPos))])
                        (columned-window
                          window
                          masterList
                          (map
                            (lambda (col)
                              (col #:rebuild
                                     (: masterList newBegPos newEndPos)))
                            allColumns)
                          highlightPos
                          newBegPos
                          newEndPos))]
                [else (display "Purposeful Error")]))]
       [(eq? method #:add-new-line) (let ([l? (>
                                                (getmaxy window)
                                                (1+ (length masterList)))])
                                      (columned-window
                                        window
                                        (append masterList (list (car xs)))
                                        (if l?
                                            (map
                                              (lambda (col)
                                                (col #:add-new-line (car xs)))
                                              allColumns)
                                          allColumns)
                                        highlightPos
                                        begPos
                                        (if l? (1+ endPos) endPos)))]
       [(eq? method #:rebuild)
             (let* ([listLen                  (length masterList)]
                    [remaining                 (- listLen begPos)]
                    [winHeight                   (getmaxy window)]
                    [linesHeight                   (1- winHeight)]
                    [currSongIndex   (1- (+ begPos highlightPos))]
                    [winSmaller         (> remaining linesHeight)]
                    [winHalf       (inexact->exact
                                     (round (/ linesHeight 2.0)))]
                    [newBegPos     (if (> (- listLen begPos) linesHeight)
                                       (cond
                                        [(< (- currSongIndex winHalf) 0)
                                              0]
                                        [(<= (-
                                               listLen
                                               currSongIndex) winHalf)
                                              (- listLen linesHeight)]
                                        [else (- currSongIndex winHalf)])
                                     begPos)]
                    [newEndPos  (if winSmaller
                                    (+ newBegPos linesHeight)
                                  listLen)])
               (columned-window
                 window
                 masterList
                 (build-columns
                   window
                   (: masterList newBegPos newEndPos)
                   allColumns)
                 (if (> (- listLen begPos) linesHeight)
                     (- (1+ currSongIndex) newBegPos)
                   highlightPos)
                 newBegPos
                 newEndPos))])))



  (columned-window win '() (build-columns win #f captions) 0 0 0))
