#!/usr/local/bin/guile
!#
(use-modules (srfi srfi-1) (ice-9 threads) (ice-9 atomic))

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

(define (windows::build-columned-window win mpd . captions)
  (define (clear-lines win numberOfLines startingVertIndex startingHorizIndex)
    (for-each (lambda (lineIndex)
                (move win lineIndex startingHorizIndex)
                (clrtoeol win)) (iota numberOfLines startingVertIndex)))
  (define (build-columns wind elements columnsOrCaptions playWinHt)
    (let ([windowWidth  (getmaxx wind)]
          [captionTotal (if elements
                            -1
                          (fold
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
                                 (- windowWidth offset)))) result))
          (let* ([cc  (car current)]
                 [col (if elements
                          (cc #:rebuild-with-size elements offset #f playWinHt)
                        (let ([ccs (car cc)])
                          (column wind (cdr cc) ccs
                                  '()  offset   (/ (string-length
                                                     ccs) captionTotal))))])
            (loop
              (cons col result)
              (cdr current)
              (+ (col #:get-width) offset)))))))

  (define* (column window function header
                   lines  offset   percentage #:optional [refinedLines #f]
                                                         [formatHeader #f]
                                                         [formatted    #f])
    (define body        (if refinedLines
                            refinedLines
                          (map
                            (lambda (line)
                              (function
                                (assoc-ref line (string->symbol header))))
                            lines)))
    (define columnWidth (if (integer? percentage)
                            percentage
                          (round (* percentage (getmaxx window)))))

    (define (check-height newLinesLength playWindowHeight)
      (let ([linesLength (1- (- (getmaxy window) playWindowHeight))])
        (when (> newLinesLength linesLength)
          (error (string-append
                   "In procedure column#:add-new-line: additional line "
                   "pushes lines length to larger value "
                   "(" (number->string newLinesLength) ") than window height "
                   "(" (number->string    linesLength) ")")))))
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
         [else                                        line])))
    (define (format-and-add line index)
      (let ([newLine (calc-new-line line)])
        (addstr window newLine #:y index #:x offset)

        newLine))

    (let ([newHeader (if formatHeader formatHeader (format-and-add header 0))]
          [newLines  (if formatted
                         formatted
                       (map format-and-add body (iota (length body) 1)))]
          [linesLen  (length lines)])
      (lambda (method . xs)
        (cond
         [(eq? method #:get-width)                                 columnWidth]
         [(eq? method #:get-tag)                       (string->symbol header)]
         [(eq? method #:get-header)                                     header]
         [(eq? method #:get-formed-header)                           newHeader]
         [(eq? method #:get-lines)                                       lines]
         [(eq? method #:get-refined)                                      body]
         [(eq? method #:get-formed-lines)                             newLines]
         [(eq? method #:calc-new-line)                           calc-new-line]
         [(eq? method #:highlight-column)  (for-each
                                             (lambda (index)
                                               (chgat window        columnWidth
                                                      (if (cadr xs)
                                                          A_REVERSE
                                                        A_NORMAL)   0
                                                      #:x offset    #:y index))
                                             (iota
                                               (- (getmaxy window) (car xs))))]
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
                                               function
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
                                                         (function refLine)
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
                                             function
                                             header
                                             (car xs)
                                             offset
                                             percentage)]
         [(eq? method #:rebuild-with-size) (check-height
                                             (length (car xs))
                                             (cadddr xs))

                                           (column
                                             window
                                             function
                                             header
                                             (car  xs)
                                             (cadr xs)
                                             (let ([perc (caddr xs)])
                                               (if perc
                                                   perc
                                                 percentage)))]
         [(eq? method #:rebuild-manually)  (let ([perc   (car   xs)]
                                                 [offSet (cadr  xs)]
                                                 [clrHt  (caddr xs)])
                                             (clear-lines
                                               window
                                               (if clrHt
                                                   clrHt
                                                 (1+ (length lines)))
                                               0
                                               offSet)
                                             (column
                                               window
                                               function
                                               header
                                               lines
                                               offSet
                                               (if (integer? percentage)
                                                   (- (getmaxx window) offSet)
                                                 (+ percentage perc))))]))))

  (define (columned-window window       playWindow mpdClient
                           masterList   allColumns isInSelectionMode
                           highlightPos begPos     endPos)
    (define (calculate-height)
      (- (getmaxy window) (playWindow #:get-height)))

    (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
    (if (>= highlightPos (calculate-height))
        (error highlightPos)
      (when (not (car isInSelectionMode))
        (chgat window -1 A_REVERSE 0 #:x 0 #:y highlightPos)))

    (refresh window)

    (lambda (method . xs)
      (cond
       [(eq? method #:get-window)                    window]
       [(eq? method #:get-max-y-x)        (getmaxyx window)]
       [(eq? method #:get-max-y)          (getmaxy  window)]
       [(eq? method #:get-max-x)          (getmaxx  window)]
       [(eq? method #:is-in-mode)   (car isInSelectionMode)]
       [(eq? method #:refresh)            (refresh  window)]
       [(eq? method #:set-vol)      (mpd-connect mpdClient)
                                    (let ([newVol ((if (cadr xs) + -)
                                                    (assoc-ref
                                                      (get-mpd-response
                                                        (mpdStatus::status
                                                          mpdClient))
                                                      'volume)
                                                    (car xs))])
                                      (mpdPlaybackOption::set-vol!
                                        mpdClient
                                        (cond
                                         [(> newVol 100)    100]
                                         [(< newVol   0)      0]
                                         [else           newVol])))
                                    (mpd-disconnect mpdClient)]
       [(eq? method #:play)         (when (> highlightPos 0)
                                      (mpd-connect mpdClient)
                                      (mpdPlaybackControl::play
                                        mpdClient
                                        (+ begPos (1- highlightPos)))
                                      (mpd-disconnect mpdClient)
                                      (playWindow #:rebuild-play mpdClient))]
       [(eq? method #:toggle-play)  (mpd-connect mpdClient)
                                    (if (string=?
                                          (assoc-ref
                                            (get-mpd-response
                                              (mpdStatus::status mpdClient))
                                            'state)
                                          "play")
                                        (mpdPlaybackControl::pause
                                          mpdClient
                                          #t)
                                      (mpdPlaybackControl::pause mpdClient #f))
                                    (mpd-disconnect mpdClient)
                                    (playWindow #:rebuild-pause mpdClient)]
       [(eq? method #:seek)         (mpd-connect mpdClient)
                                    (mpdPlaybackControl::seek-current
                                      mpdClient
                                      (car xs))
                                    (mpd-disconnect mpdClient)]
       [(eq? method #:enter-select) (chgat window -1    A_NORMAL
                                           0      #:x 0 #:y highlightPos)
                                    ((car allColumns)
                                      #:highlight-column
                                        (playWindow #:get-height) #t)
                                    (columned-window
                                      window       playWindow mpdClient
                                      masterList   allColumns (cons #t 0)
                                      highlightPos begPos     endPos)]
       [(eq? method #:move-select)  (when (not (car isInSelectionMode))
                                      (error
                                        (string-append
                                          "In procedure "
                                          "columned-window#:move-select: "
                                          "can't move selected columns "
                                          "while not in Selection Mode.")))
                                    (let* ([moveAmount (car xs)]
                                           [moveIsNeg   (negative? moveAmount)]
                                           [lastIndex   (1-
                                                          (length allColumns))]
                                           [oldIndex   (cdr isInSelectionMode)]
                                           [newIndex   (+ oldIndex moveAmount)]
                                           [realNewInd (cond
                                                        [(< newIndex 0)
                                                              0]
                                                        [(> newIndex lastIndex)
                                                              lastIndex]
                                                        [else newIndex])]
                                           [playHeight (playWindow
                                                         #:get-height)])
                                      ((list-ref allColumns oldIndex)
                                        #:highlight-column playHeight #f)
                                      ((list-ref allColumns realNewInd)
                                        #:highlight-column playHeight #t)
                                      (columned-window
                                        window       playWindow mpdClient
                                        masterList   allColumns (cons
                                                                  #t
                                                                  realNewInd)
                                        highlightPos begPos     endPos))]
       [(eq? method #:change-select)
             (when (not (car isInSelectionMode))
               (error (string-append
                        "In procedure columned-window#:move-select: can't "
                        "increase selected column while not in Selection "
                        "Mode.")))
             (columned-window
               window
               playWindow
               mpdClient
               masterList
               (let ([index      (cdr isInSelectionMode)]
                     [lastIndex (1- (length allColumns))])
                 (if (= index lastIndex)
                     allColumns
                   (let* ([incPerc (/       (car xs)    (getmaxx window))]
                          [decPerc (/ (* incPerc -1) (- lastIndex index))])
                     (let loop ([result         '()]
                                [current allColumns]
                                [offset           0]
                                [count            0])
                       (if (null? current)
                           (reverse result)
                         (let* ([pwHeight   (playWindow #:get-height)]
                                [hghlght              (= count index)]
                                [newCol   (cond
                                           [(< count index)
                                                 (car current)]
                                           [hghlght
                                                 ((car current)
                                                   #:rebuild-manually
                                                     incPerc
                                                     offset
                                                     (-
                                                       (getmaxy window)
                                                       pwHeight))]
                                           [else ((car current)
                                                   #:rebuild-manually
                                                     decPerc
                                                     offset
                                                     #f)])])
                           (when hghlght
                             (newCol #:highlight-column pwHeight #t))

                           (loop
                             (cons newCol result)
                             (cdr current)
                             (+ (newCol #:get-width) offset)
                             (1+ count))))))))
               isInSelectionMode
               highlightPos
               begPos
               endPos)]
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
                        mpdClient
                        masterList
                        allColumns
                        isInSelectionMode
                        (if (and (negative? newPos) (zero? highlightPos))
                            0
                          (if (< newPos 1) 1 (if (< listLen newPos)
                                                 (1- listLen)
                                               newPos)))
                        begPos
                        endPos)]
                [(or
                   (and
                     (> newPos       lastVisibleLineOfWin)
                     (= highlightPos lastVisibleLineOfWin)
                     (= endPos       listLen))
                   (and
                     (< newPos       1)
                     (= highlightPos 1)
                     (= begPos       0))
                   (and
                     (< newPos       0)
                     (= highlightPos 0)))
                      (columned-window window       playWindow
                                       mpdClient    masterList
                                       allColumns   isInSelectionMode
                                       highlightPos begPos            endPos)]
                [(and (< newPos 1) (< (+ begPos (car xs)) 0))
                      (clear-lines window lastVisibleLineOfWin 1 0)
                      (columned-window
                        window
                        playWindow
                        mpdClient
                        masterList
                        (map
                          (lambda (col)
                            (col #:rebuild (: masterList 0 (1- winLen))))
                          allColumns)
                        isInSelectionMode
                        1
                        0
                        (if (< (- listLen begPos) (1- winLen))
                            listLen
                          (1- winLen)))]
                [(and
                   (> newPos lastVisibleLineOfWin)
                   (> (+ begPos newPos) listLen))
                      (let ([newBegPos (if (< (- listLen begPos) (1- winLen))
                                           begPos
                                         (- listLen (1- winLen)))]
                            [newEndPos listLen])
                        (clear-lines window lastVisibleLineOfWin 1 0)
                        (columned-window
                          window
                          playWindow
                          mpdClient
                          masterList
                          (map
                            (lambda (col)
                              (col #:rebuild
                                     (: masterList newBegPos newEndPos)))
                            allColumns)
                          isInSelectionMode
                          (- newEndPos newBegPos)
                          newBegPos
                          newEndPos))]
                [(and
                   (> newPos lastVisibleLineOfWin)
                   (< (- listLen (+ begPos (car xs))) (1- winLen)))
                      (let ([newBegPos (if (< (- listLen begPos) (1- winLen))
                                           begPos
                                         (- listLen (1- winLen)))]
                            [newEndPos listLen])
                        (clear-lines window lastVisibleLineOfWin 1 0)
                        (columned-window
                          window
                          playWindow
                          mpdClient
                          masterList
                          (map
                            (lambda (col)
                              (col #:rebuild
                                     (: masterList newBegPos newEndPos)))
                            allColumns)
                          isInSelectionMode
                          (- winLen (- listLen (+ (1- begPos) newPos)))
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
                        (clear-lines window lastVisibleLineOfWin 1 0)
                        (columned-window
                          window
                          playWindow
                          mpdClient
                          masterList
                          (map
                            (lambda (col)
                              (col #:rebuild
                                     (: masterList newBegPos newEndPos)))
                            allColumns)
                          isInSelectionMode
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
                                                          (1- winHeight)))]
                                           [inc?      (<
                                                        (- masterLen begPos)
                                                        (1- winHeight))])
                                      (columned-window
                                        window
                                        playWindow
                                        mpdClient
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
                                        isInSelectionMode
                                        highlightPos
                                        begPos
                                        (if inc? (1+ endPos) endPos)))]
       [(eq? method #:rebuild)
             (erase window)
             (let* ([pw               (playWindow #:rebuild-size)]
                    [listLen                  (length masterList)]
                    [remaining                 (- listLen begPos)]
                    [winHeight                 (calculate-height)]
                    [linesHeight                   (1- winHeight)]
                    [currSongIndx    (1- (+ begPos highlightPos))]
                    [winSmaller         (> remaining linesHeight)]
                    [winHalf            (round (/ linesHeight 2))]
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
                 mpdClient
                 masterList
                 (build-columns
                   window
                   (: masterList newBegPos newEndPos)
                   allColumns
                   (pw #:get-height))
                 isInSelectionMode
                 (if (> (- listLen begPos) linesHeight)
                     (- (1+ currSongIndx) newBegPos)
                   highlightPos)
                 newBegPos
                 newEndPos))])))

  (define* (play-window window runningThread sBox dBox #:optional [height 3])
    (define (calc-progress-bar elapsed totalTime stopped?)
      (define (parse-seconds seconds)
        (let* ([rounded        (inexact->exact (round seconds))]
               [mins    (number->string (quotient  rounded 60))]
               [secs    (number->string (remainder rounded 60))])
          (string-append mins ":" (if (= (string-length secs) 1)
                                      (string-append "0" secs)
                                    secs))))

      (let* ([remaining                            (- totalTime elapsed)]
             [eString                          (parse-seconds   elapsed)]
             [rString                          (parse-seconds remaining)]
             [tString                          (parse-seconds totalTime)]
             [totalLen                     (- (getmaxx win)           3
                                              (string-length eString) 4
                                              (string-length rString) 3
                                              (string-length tString) 1)]
             [firstLen  (if (zero? totalTime)
                            0
                          (inexact->exact
                            (floor (* (/ elapsed totalTime) totalLen))))])
        (string-append
          eString
          " ["
          (string-concatenate/shared
            (make-list (pos-or-to-zero firstLen)                "="))
          (if stopped? "-" ">")
          (string-concatenate/shared
            (make-list (pos-or-to-zero (- totalLen firstLen 1)) "-"))
          "] "
          rString
          " / "
          tString)))

    (define (write-lines wind windHeightDiff statusStrings rev?)
      (define (write-line lineIndex lineStrings winLength r?)
        (let ([x (fold
                   (lambda (elem ret)
                     (addchstr wind          (inverse-on
                                               ((cdr elem) (car elem)))
                               #:y lineIndex #:x ret)

                     (+ ret (string-length (car elem))))
                   0
                   lineStrings)])
          (addchstr
            wind
            ((if r? inverse-on inverse-off)
              (string-concatenate/shared
                (make-list (pos-or-to-zero (- winLength x)) " ")))
            #:y lineIndex
            #:x x)))

      (if rev?
          (write-line windHeightDiff statusStrings (getmaxx wind) rev?)
        (clear-lines wind height windHeightDiff 0)))

    (define (set-display win       client
                         statusBox displayedSongBox heightMeasurement)
      (mpd-connect client)
      (let ([status (get-mpd-response (mpdStatus::status client))])
        (mpd-disconnect client)

        (let ([stateString           (assoc-ref status 'state)]
              [dh          (- (getmaxy win) heightMeasurement)])
          (cond
           [(string=? stateString "stop")
                 (let ([winWidth              (getmaxx win)]
                       [prevInfo (atomic-box-ref statusBox)]
                       [dispSong    (list (cons "" normal))])
                   (write-lines win dh      dispSong #t)
                   (when (or
                           (not prevInfo)
                           (not (and
                                  (= winWidth (car prevInfo))
                                  (string=?
                                    (substring (caaadr prevInfo) 0 3)
                                    " ▪ "))))
                     (let ([status (list (cons
                                           (string-append
                                             " ▪ "
                                             (calc-progress-bar 0 0 #t))
                                           normal))])
                       (write-lines win (1+ dh) status   #t)
                       (atomic-box-set!    statusBox (list
                                                       winWidth
                                                       status
                                                       (cons 0 0)))))
                   (atomic-box-set! displayedSongBox dispSong))]
           [(string=? stateString "play")
                 (mpd-connect client)
                 (let ([song (get-mpd-response
                               (mpdStatus::current-song client))])
                   (mpd-disconnect client)
                   (let* ([elapsed  (assoc-ref status 'elapsed)]
                          [time     (assoc-ref song   'Time)]
                          [status   (list (cons (string-append
                                                  " ▶ "
                                                  (calc-progress-bar
                                                    elapsed
                                                    time
                                                    #f)) normal))]
                          [dispSong (list
                                      (cons " "                      normal)
                                      (cons (assoc-ref song 'Title)  bold)
                                      (cons " from "                 normal)
                                      (cons (assoc-ref song 'Album)  bold)
                                      (cons " by "                   normal)
                                      (cons (assoc-ref song 'Artist) bold))])
                     (write-lines win dh      dispSong #t)
                     (write-lines win (1+ dh) status   #t)
                     (atomic-box-set! statusBox        (list
                                                         (getmaxx win)
                                                         status
                                                         (cons elapsed time)))
                     (atomic-box-set! displayedSongBox dispSong)))]
           [(string=? stateString "pause")
                 (let ([winWidth              (getmaxx win)]
                       [prevInfo (atomic-box-ref statusBox)])
                   (write-lines win dh (atomic-box-ref displayedSongBox) #t)

                   (if (= winWidth (car prevInfo))
                       (let ([prevStatus (caaadr prevInfo)])
                         (when (not (string=? (substring prevStatus 0 3) " 𝍪 "))
                           (let ([status (list (cons
                                                 (string-append
                                                   " 𝍪 "
                                                   (substring prevStatus 3))
                                                 (cdaadr prevInfo)))])
                             (write-lines win (1+ dh) status #t)
                             (atomic-box-set! statusBox (list
                                                          winWidth
                                                          status
                                                          (caddr prevInfo))))))
                     (let* ([prevTimes (caddr prevInfo)]
                            [status    (list (cons (string-append
                                                     " 𝍪 "
                                                     (calc-progress-bar
                                                       (car prevTimes)
                                                       (cdr prevTimes)
                                                       #f)) normal))])
                       (write-lines win (1+ dh) status #t)
                       (atomic-box-set! statusBox (list
                                                    winWidth
                                                    status
                                                    prevTimes)))))])
          (refresh win)))
      (sleep 1)
      (set-display win client statusBox displayedSongBox heightMeasurement))

    (let ([diff (- (getmaxy window) height)]
          [stup (if runningThread
                    runningThread
                  (call-with-new-thread
                    (lambda ()
                      (set-display window (new-mpd-client)
                                   sBox   dBox             height))))])

      (lambda (method . xs)
        (cond
         [(eq? method #:get-height)                              height]
         [(eq? method #:rebuild-play)  (write-lines window diff #f #f)
                                       (let ([stat     (atomic-box-ref sBox)]
                                             [playHght  (- (getmaxy
                                                             window) height)])
                                         (write-lines
                                           window
                                           playHght
                                           (atomic-box-ref dBox)
                                           #t)
                                         (write-lines
                                           window
                                           (1+ playHght)
                                           (list
                                             (cons
                                               (string-append
                                                 " ▶"
                                                 (substring (caaadr stat) 2))
                                               (cdaadr stat)))
                                           #t))
                                       (play-window window stup
                                                    sBox   dBox height)]
         [(eq? method #:rebuild-pause) (write-lines window diff #f #f)
                                       (let* ([playHght  (- (getmaxy
                                                              window) height)]
                                              [stat     (atomic-box-ref sBox)]
                                              [state            (caaadr stat)]
                                              [sym      (substring state 0 2)])
                                         (write-lines
                                           window
                                           playHght
                                           (atomic-box-ref dBox)
                                           #t)
                                         (write-lines
                                           window
                                           (1+ playHght)
                                           (list
                                             (cons
                                               (string-append
                                                 (cond
                                                  [(string=? sym " 𝍪") " ▶"]
                                                  [(string=? sym " ▶") " 𝍪"]
                                                  [else                " ▪"])
                                                 (substring state 2))
                                               (cdaadr stat)))
                                           #t))
                                       (play-window window stup
                                                    sBox   dBox height)]
         [(eq? method #:rebuild-size)  ;; (write-lines window diff #f #f)
                                       (let* ([playHght   (- (getmaxy
                                                               window) height)]
                                              [prevInfo  (atomic-box-ref sBox)]
                                              [prevTimes      (caddr prevInfo)]
                                              [oldStat       (caaadr prevInfo)]
                                              [newStat   (list
                                                           (cons
                                                             (string-append
                                                               (substring
                                                                 oldStat
                                                                 0
                                                                 3)
                                                               (calc-progress-bar
                                                                 (car prevTimes)
                                                                 (cdr prevTimes)
                                                                 (string=?
                                                                   (substring
                                                                     oldStat
                                                                     0
                                                                     2)
                                                                   " ▪")))
                                                             (cdaadr
                                                               prevInfo)))])
                                         (write-lines
                                           window
                                           playHght
                                           (atomic-box-ref dBox)
                                           #t)
                                         (write-lines window  (1+ playHght)
                                                      newStat #t)
                                         (atomic-box-set!
                                           sBox
                                           (list (getmaxx
                                                   window) newStat prevTimes)))
                                       (play-window window stup
                                                    sBox   dBox height)]))))



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
