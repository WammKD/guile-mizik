#!/usr/local/bin/guile
!#
(use-modules (srfi srfi-1) (ice-9 threads))

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

(define (windows::build-columned-window win mpd . captions)
  (define (build-columns wind elements columnsOrCaptions playWinHt)
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
                                                (- windowWidth offset)
                                                playWinHt)
                             (column
                               wind
                               (car current)
                               '()
                               offset
                               (- windowWidth offset))) result))
          (let* ([cc  (car current)]
                 [col (if elements
                          (cc #:rebuild-with-size elements offset #f playWinHt)
                        (column
                          wind
                          cc
                          '()
                          offset
                          (/ (string-length cc) captionTotal)))])
            (loop
              (cons col result)
              (cdr current)
              (+ (col #:get-width) offset)))))))

  (define* (column window header lines offset percentage #:optional
                                                           [refinedLines #f]
                                                           [formatHeader #f]
                                                           [formatted    #f])
    (define body        (if refinedLines
                            refinedLines
                          (map
                            (lambda (line)
                              (assoc-ref line (string->symbol header)))
                            lines)))
    (define columnWidth (if (exact? percentage)
                            percentage
                          (inexact->exact
                            (round (* percentage (getmaxx window))))))

    (define (check-height newLinesLength playWindowHeight)
      (let ([linesLength (1- (- (getmaxy window) playWindowHeight))])
        (when (> newLinesLength linesLength)
          (error (string-append
                   "In procedure column#:add-new-line: additional line "
                   "pushes lines length to larger value "
                   "(" (number->string newLinesLength) ") than window height "
                   "(" (number->string linesLength)    ")")))))
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

    (let ([newHeader (if formatHeader formatHeader (format-and-add header 0))]
          [newLines  (if formatted
                         formatted
                       (map format-and-add body (iota (length body) 1)))]
          [linesLen (length lines)])
      (lambda (method . xs)
        (cond
         [(eq? method #:get-width)                            columnWidth]
         [(eq? method #:get-tag)                  (string->symbol header)]
         [(eq? method #:get-header)                                header]
         [(eq? method #:get-formed-header)                      newHeader]
         [(eq? method #:get-lines)                                  lines]
         [(eq? method #:get-refined)                                 body]
         [(eq? method #:get-formed-lines)                        newLines]
         [(eq? method #:calc-new-line)                      calc-new-line]
         [(eq? method #:add-new-line)      (let* ([line            (car xs)]
                                                  [index       (if (>
                                                                     (cadr xs)
                                                                     linesLen)
                                                                   linesLen
                                                                 (cadr xs))]
                                                  [i+1           (1+ index)]
                                                  [newLen  (if (caddr xs)
                                                               linesLen
                                                             (1- linesLen))]
                                                  [refLine (assoc-ref
                                                             line
                                                             (string->symbol
                                                               header))])
                                             (check-height newLen (cadddr xs))

                                             (column
                                               window
                                               header
                                               (append
                                                 (: lines 0     index)
                                                 (list line)
                                                 (: lines index newLen))
                                               offset
                                               percentage
                                               (append
                                                 (: body 0     index)
                                                 (list refLine)
                                                 (: body index newLen))
                                               newHeader
                                               (append
                                                 (: newLines 0 index)
                                                 (list (format-and-add
                                                         refLine
                                                         (1+ index)))
                                                 (let ([sub (:
                                                              body
                                                              index
                                                              newLen)])
                                                   (map
                                                     format-and-add
                                                     sub
                                                     (iota
                                                       (length sub)
                                                       (2+ index)))))))]
         [(eq? method #:rebuild)           (column
                                             window
                                             header
                                             (car xs)
                                             offset
                                             percentage)]
         [(eq? method #:rebuild-with-size) (check-height
                                             (length (car xs))
                                             (cadddr xs))

                                           (column
                                             window
                                             header
                                             (car  xs)
                                             (cadr xs)
                                             (let ([perc (caddr xs)])
                                               (if perc
                                                   perc
                                                 percentage)))]))))

  (define (columned-window window     playWindow   masterList
                           allColumns highlightPos begPos     endPos)
    (define (calculate-height)
      (- (getmaxy window) (playWindow #:get-height)))

    (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
    (if (>= highlightPos (calculate-height))
        (error highlightPos)
      (chgat window -1 A_REVERSE 0 #:x 0 #:y highlightPos))

    (lambda (method . xs)
      (cond
       [(eq? method #:get-window)              window]
       [(eq? method #:get-max-y-x)  (getmaxyx window)]
       [(eq? method #:get-max-y)    (getmaxy  window)]
       [(eq? method #:get-max-x)    (getmaxx  window)]
       [(eq? method #:refresh)      (refresh  window)]
       [(eq? method #:play)         (when (> highlightPos 0)
                                      (mpd-connect    (car xs))
                                      (mpdPlaybackControl::play
                                        (car xs)
                                        (+ begPos (1- highlightPos)))
                                      (mpd-disconnect (car xs))
                                      (playWindow #:rebuild (car xs)))]
       [(eq? method #:toggle-play)  (mpd-connect    (car xs))
                                    (if (string=?
                                          (assoc-ref
                                            (get-mpd-response
                                              (mpdStatus::status client))
                                            'state)
                                          "play")
                                        (mpdPlaybackControl::pause client #t)
                                      (mpdPlaybackControl::pause client #f))
                                    (mpd-disconnect (car xs))
                                    (playWindow #:rebuild (car xs))]
       [(eq? method #:move-cursor)
             (let ([newPos               (+ highlightPos (car xs))]
                   [winLen                      (calculate-height)]
                   [listLen                    (length masterList)]
                   [lastVisibleLineOfWin         (- endPos begPos)])
               (when (not (= highlightPos 0))
                 (chgat window -1 A_NORMAL 0 #:x 0 #:y highlightPos))

               (cond
                [(between? 0 newPos (1+ lastVisibleLineOfWin))
                      (columned-window
                        window
                        playWindow
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
                      (columned-window window       playWindow
                                       masterList   allColumns
                                       highlightPos begPos     endPos)]
                [(and (< newPos 1) (< (+ begPos (car xs)) 0))
                      (columned-window
                        window
                        playWindow
                        masterList
                        (map
                          (lambda (col)
                            (col #:rebuild (: masterList 0 (1- winLen))))
                          allColumns)
                        1
                        0
                        (if (< (- listLen begPos) (1- winLen))
                            listLen
                          (1- winLen)))]
                [(and
                   (> newPos lastVisibleLineOfWin)
                   (> (+ begPos highlightPos (car xs)) listLen))
                      (let ([newBegPos (if (< (- listLen begPos) (1- winLen))
                                           begPos
                                         (- listLen (1- winLen)))]
                            [newEndPos (if (< (- listLen begPos) (1- winLen))
                                           listLen
                                         (2+ winLen))])
                        (columned-window
                          window
                          playWindow
                          masterList
                          (map
                            (lambda (col)
                              (col #:rebuild
                                     (: masterList newBegPos newEndPos)))
                            allColumns)
                          (- newEndPos newBegPos)
                          newBegPos
                          newEndPos))]
                [(or (< newPos 1) (> newPos lastVisibleLineOfWin))
                      (let ([newBegPos (+ begPos (car xs))]
                            [newEndPos (if (and
                                             (< newPos highlightPos)
                                             (not (=
                                                    (- endPos begPos)
                                                    (1- winLen))))
                                           endPos
                                         (+ endPos (car xs)))])
                        (columned-window
                          window
                          playWindow
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
       [(eq? method #:add-new-line) (let* ([line                 (car xs)]
                                           [masterLen (length masterList)]
                                           [winHeight  (calculate-height)]
                                           [index     (if (cadr xs)
                                                          (cadr xs)
                                                        masterLen)]
                                           [l?        (and
                                                        (>= index begPos)
                                                        (<
                                                          (- index begPos)
                                                          winHeight))]
                                           [inc?      (<
                                                        (- masterLen begPos)
                                                        (1- winHeight))])
                                      (columned-window
                                        window
                                        playWindow
                                        (append
                                          (: masterList 0 index)
                                          (list line)
                                          (: masterList index))
                                        (if l?
                                            (let ([modPos (- index begPos)])
                                              (map
                                                (lambda (col)
                                                  (col #:add-new-line
                                                         line modPos
                                                         inc? (playWindow
                                                                #:get-height)))
                                                allColumns))
                                          allColumns)
                                        highlightPos
                                        begPos
                                        (if inc? (1+ endPos) endPos)))]
       [(eq? method #:rebuild)
             (let* ([pw                    (playWindow #:rebuild)]
                    [listLen                  (length masterList)]
                    [remaining                 (- listLen begPos)]
                    [winHeight                 (calculate-height)]
                    [linesHeight                   (1- winHeight)]
                    [currSongIndx    (1- (+ begPos highlightPos))]
                    [winSmaller         (> remaining linesHeight)]
                    [winHalf       (inexact->exact
                                     (round (/ linesHeight 2.0)))]
                    [newBegPos    (if (> (- listLen begPos) linesHeight)
                                      (cond
                                       [(< (- currSongIndx winHalf) 0)
                                             0]
                                       [(<= (- listLen currSongIndx) winHalf)
                                             (- listLen linesHeight)]
                                       [else (- currSongIndx winHalf)])
                                    begPos)]
                    [newEndPos    (if winSmaller 
                                      (+ newBegPos linesHeight)
                                    listLen)])
               (columned-window
                 window
                 pw
                 masterList
                 (build-columns
                   window
                   (: masterList newBegPos newEndPos)
                   allColumns
                   (pw #:get-height))
                 (if (> (- listLen begPos) linesHeight)
                     (- (1+ currSongIndx) newBegPos)
                   highlightPos)
                 newBegPos
                 newEndPos))])))

  (define* (play-window window runningThread #:optional [height 2])
    (define (write-lines wind windHeightDiff statusString rev?)
      (define (write-line lineIndex lineString winLength state)
        (addstr
          wind
          (string-concatenate
            (cons lineString (make-list (- winLength (string-length
                                                       lineString)) " ")))
          #:y lineIndex
          #:x 0)

        (chgat window -1 state 0 #:x 0 #:y lineIndex))

      (if rev?
          (write-line windHeightDiff statusString (getmaxx wind) A_REVERSE)
        (let ([windowLength (getmaxx wind)])
          (for-each
            (lambda (lineHeight)
              (when (< lineHeight (getmaxy wind))
                (write-line lineHeight "" windowLength A_NORMAL)))
            (iota height windHeightDiff)))))

    (define (set-display win client heightMeasurement loop?)
      (define (parse-seconds seconds)
        (let* ([rounded        (inexact->exact (floor seconds))]
               [mins    (number->string (quotient  rounded 60))]
               [secs    (number->string (remainder rounded 60))])
          (string-append mins ":" (if (= (string-length secs) 1)
                                      (string-append "0" secs)
                                    secs))))

      (mpd-connect client)
      (let ([status (get-mpd-response (mpdStatus::status       client))]
            [song   (get-mpd-response (mpdStatus::current-song client))])
        (mpd-disconnect client)

        (let ([state   (let ([stateString (assoc-ref status 'state)])
                         (cond
                          [(string=? stateString "stop")  " ▪ "]
                          [(string=? stateString "play")  " ▶ "]
                          [(string=? stateString "pause") " 𝍪 "]))]
              [elapsed                 (assoc-ref status 'elapsed)]
              [time                    (assoc-ref song   'Time)   ])
          (write-lines
            win
            (- (getmaxy win) heightMeasurement)
            (string-append state (if elapsed
                                     (parse-seconds elapsed)
                                   "0:00") " / " (if time
                                                     (parse-seconds time)
                                                   "0:00"))
            #t)
          (refresh win)))

      (when loop?
        (sleep 1)
        (set-display win client heightMeasurement loop?)))

    (let ([diff (- (getmaxy window) height)]
          [stup (if runningThread
                    runningThread
                  (call-with-new-thread (lambda ()
                                          (set-display window (new-mpd-client)
                                                       height #t))))])

      (lambda (method . xs)
        (cond
         [(eq? method #:get-height)                      height]
         [(eq? method #:rebuild)    (write-lines window diff #f #f)
                                    (call-with-new-thread
                                      (lambda ()
                                        (set-display window (new-mpd-client)
                                                     height #f)))
                                    (play-window window stup height)]))))



  (columned-window win (play-window   win #f)
                   '() (build-columns win #f  captions #f)
                   0   0                                   0))
