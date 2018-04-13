#!/usr/local/bin/guile
!#
(use-modules (srfi srfi-1) (ice-9 threads) (ice-9 atomic))
(include     "./util.scm")

(define (windows::build-columned-window stdscr mpd . captions)
  (define (clear-lines win numberOfLines startingVertIndex startingHorizIndex)
    (for-each
      (lambda (lineIndex) (move win lineIndex startingHorizIndex) (clrtoeol win))
      (iota numberOfLines startingVertIndex)))

  (define (calc-column-width percentage)
    (floor (* percentage (cols))))

  (define (build-columns window headers)
    (let ([windowWidth          (cols)]
          [totalLenOfEachHeader (fold
                                  (lambda (header total)
                                    (+ (string-length (car header)) total))
                                  0
                                  headers)])
      (let loop ([result               '()]
                 [remainingHeaders headers]
                 [offset                 0])
        (let ([header                 (car remainingHeaders)]
              [restOfRemainingHeaders (cdr remainingHeaders)])
          (if (null? restOfRemainingHeaders)
              (reverse (cons (column
                               window
                               (cdr header)
                               (car header)
                               '()
                               offset
                               (cons #f (- windowWidth offset))) result))
            (let ([col (let ([headerTitle (car header)])
                         (column
                           window
                           (cdr header)
                           headerTitle
                           '()
                           offset
                           (cons #t (/ (string-length
                                         headerTitle) totalLenOfEachHeader))))])
              (loop
                (cons col result)
                restOfRemainingHeaders
                (+ (col #:get-width) offset))))))))
  (define (rebuild-columns window         lines
                           currentColumns playWindowHeight selectionStatus)
    (define (determine-highlight columnInQuestion columnIndex)
      (when (and
              (assq-ref selectionStatus 'status)
              (= columnIndex (assq-ref selectionStatus 'index)))
        (columnInQuestion #:highlight-column playWindowHeight #t)))

    (let loop ([result '()] [remainingColumns currentColumns]
               [offset   0] [index                         0])
      (let ([firstColumn            (car remainingColumns)]
            [restOfRemainingColumns (cdr remainingColumns)])
        (if (null? restOfRemainingColumns)
            (let ([col (firstColumn #:rebuild-with-size
                                      lines             offset
                                      (- (cols) offset) playWindowHeight)])
              (determine-highlight col index)

              (reverse (cons col result)))
          (let ([col (firstColumn
                       #:rebuild-with-size lines offset #f playWindowHeight)])
            (determine-highlight col index)

            (loop (cons col result)            restOfRemainingColumns
                  (+ (col #:get-width) offset) (1+ index)))))))

  (define* (column window format-line
                   header columnLines
                   offset percentage  #:optional [formattedLines #f]
                                                 [setHeader      #f]
                                                 [setLines       #f])
    (define body        (if formattedLines
                            formattedLines
                          (map
                            (lambda (line)
                              (format-line
                                (assoc-ref line (string->symbol header))))
                            columnLines)))
    (define columnWidth (if (car percentage)
                            (calc-column-width (cdr percentage))
                          (cdr percentage)))



    (define (check-height lengthOfNewLines playWindowHeight)
      (let ([linesLength (1- (lines))])
        (when (> lengthOfNewLines linesLength)
          (endwin)
          (error (string-append
                   "In procedure column#:add-new-line: additional line "
                   "pushes lines length to larger value "
                   "(" (number->string lengthOfNewLines) ") than window height "
                   "(" (number->string      linesLength) ")")))))
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



    (let ([newHeader (if setHeader setHeader (format-and-add header 0))]
          [newLines  (if setLines
                         setLines
                       (map format-and-add body (iota (length body) 1)))]
          [linesLen  (length columnLines)])
      (lambda (method . xs)
        (case method
          [(#:get-width)                           columnWidth]
          [(#:get-offset)                               offset]
          [(#:get-tag)                 (string->symbol header)]
          [(#:get-header)                               header]
          [(#:get-formed-header)                     newHeader]
          [(#:get-lines)                           columnLines]
          [(#:get-formatted)                              body]
          [(#:get-formed-lines)                       newLines]
          [(#:form-line-of-approp-len) form-line-of-approp-len]
          [(#:get-percentage)
                     (if (car percentage) (cdr percentage) #f)]
          [(#:highlight-column)
               (for-each
                 (lambda (index)
                   (chgat window columnWidth (if (cadr xs)
                                                 A_REVERSE
                                               A_NORMAL)   0
                          #:x    offset      #:y           index))
                 (iota (- (lines) (car xs))))]
          [(#:add-new-line)
               (let* ([line                                          (car xs)]
                      [index   (if (> (cadr xs) linesLen) linesLen (cadr xs))]
                      [i+1                                         (1+ index)]
                      [newLen          (if (caddr xs) linesLen (1- linesLen))]
                      [refLine       (assoc-ref line (string->symbol header))])
                 (check-height newLen (cadddr xs))

                 (column
                   window
                   format-line
                   header
                   (append
                     (: columnLines 0     index)
                     (list line)
                     (: columnLines index newLen))
                   offset
                   percentage
                   (append
                     (: body 0     index)
                     (list refLine)
                     (: body index newLen))
                   newHeader
                   (append
                     (: newLines 0 index)
                     (list (format-and-add (format-line refLine) (1+ index)))
                     (let ([sub (: body index newLen)])
                       (map
                         format-and-add
                         sub
                         (iota (length sub) (2+ index)))))))]
          [(#:rebuild)
                (column window format-line header (car xs) offset percentage)]
          [(#:rebuild-with-size)
                (check-height (length (car xs)) (cadddr xs))

                (column window   format-line header
                        (car xs) (cadr xs)   (let ([perc (caddr xs)])
                                               (if perc
                                                   (cons #f perc)
                                                 percentage)))]
          [(#:rebuild-manually)
               (let ([newPercentage (car   xs)]
                     [newOffset     (cadr  xs)]
                     [clrHt         (caddr xs)])
                 (clear-lines window (if clrHt
                                         clrHt
                                       (1+ (length columnLines))) 0 newOffset)

                 (column
                   window
                   format-line
                   header
                   columnLines
                   newOffset
                   (if (car percentage)
                       (cons #t newPercentage)
                     (cons #f (- (cols) newOffset)))))]))))

  (define (columned-window window       playWindow mpdClient
                           masterList   allColumns selectModeDetails
                           highlightPos begPos     endPos)
    (define (calculate-height)
      (- (lines) (playWindow #:get-height)))

    (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
    (if (>= highlightPos (calculate-height))
        (begin (endwin) (error highlightPos))
      (when (not (assq-ref selectModeDetails 'status))
        (chgat window -1 A_REVERSE 0 #:x 0 #:y highlightPos)))

    (refresh window)

    (lambda (method . xs)
      (case method
        [(#:get-window)                                 window]
        [(#:get-max-y-x)                     (getmaxyx window)]
        [(#:get-max-y)                       (getmaxy  window)]
        [(#:get-max-x)                       (getmaxx  window)]
        [(#:is-in-mode)   (assq-ref selectModeDetails 'status)]
        [(#:refresh)                         (refresh  window)]
        [(#:set-vol)      (mpd-connect mpdClient)
                          (let ([newVol ((if (cadr xs) + -)
                                          (assoc-ref
                                            (get-mpd-response
                                              (mpdStatus::status mpdClient))
                                            'volume)
                                          (car xs))])
                            (mpdPlaybackOption::set-vol!
                              mpdClient
                              (cond
                               [(> newVol 100)    100]
                               [(< newVol   0)      0]
                               [else           newVol])))
                          (mpd-disconnect mpdClient)]
        [(#:play)         (when (> highlightPos 0)
                            (mpd-connect mpdClient)
                            (mpdPlaybackControl::play
                              mpdClient
                              (+ begPos (1- highlightPos)))
                            (mpd-disconnect mpdClient)
                            (playWindow #:rebuild-play mpdClient))]
        [(#:toggle-play)  (mpd-connect mpdClient)
                          (if (string=? (assoc-ref
                                          (get-mpd-response
                                            (mpdStatus::status mpdClient))
                                          'state) "play")
                              (mpdPlaybackControl::pause mpdClient #t)
                            (mpdPlaybackControl::pause mpdClient #f))
                          (mpd-disconnect mpdClient)
                          (playWindow #:rebuild-pause mpdClient)]
        [(#:seek)         (mpd-connect mpdClient)
                          (mpdPlaybackControl::seek-current mpdClient (car xs))
                          (mpd-disconnect mpdClient)]
        [(#:enter-select) (chgat window -1 A_NORMAL 0 #:x 0 #:y highlightPos)
                          ((car allColumns) #:highlight-column
                                              (playWindow #:get-height) #t)
                          (columned-window
                            window
                            playWindow
                            mpdClient
                            masterList
                            allColumns
                            (alist 'status      #t
                                   'index       0
                                   'sortedIndex (assq-ref
                                                  selectModeDetails
                                                  'sortedIndex))
                            highlightPos
                            begPos
                            endPos)]
        [(#:leave-select)
              (when (not (assq-ref selectModeDetails 'status))
                (endwin)
                (error (string-append
                         "In procedure columned-window#:leave-select: can't "
                         "exit Selection Mode while not in Selection Mode.")))

              ((list-ref allColumns (assq-ref selectModeDetails 'index))
                #:highlight-column (playWindow #:get-height) #f)

              (columned-window
                window       playWindow
                mpdClient    masterList
                allColumns   (alist 'status      #f
                                    'index       #f
                                    'sortedIndex (assq-ref
                                                   selectModeDetails
                                                   'sortedIndex))
                highlightPos begPos                                  endPos)]
        [(#:sort-select)
              (when (not (assq-ref selectModeDetails 'status))
                (endwin)
                (error (string-append
                         "In procedure columned-window#:sort-select: can't "
                         "sort a column while not in Selection Mode.")))

              (let* ([colIndex             (assq-ref selectModeDetails 'index)]
                     [colSymbol     ((list-ref allColumns colIndex) #:get-tag)]
                     [sortingFunct (if (number? (assq-ref
                                                  (car masterList)
                                                  colSymbol))      < string<?)]
                     [newMasterLst (stable-sort
                                     masterList
                                     (lambda (song1 song2)
                                       (sortingFunct
                                         (assq-ref song1 colSymbol)
                                         (assq-ref song2 colSymbol))))]
                     [newSelecDets (alist 'status      #t
                                          'index       colIndex
                                          'sortedIndex colIndex)]
                     [lengthOfNML  (length newMasterLst)])
                (mpd-connect mpdClient)
                (for-each
                  (lambda (song indexToUse)
                    (mpdPlaylistCurrent::move-id!
                      mpdClient (assq-ref song 'Id) indexToUse))
                  newMasterLst
                  (iota lengthOfNML))
                (mpd-disconnect mpdClient)

                (let* ([highlightIndex    (1- (+ begPos highlightPos))]
                       [newHighlightIndex
                             (if (= highlightPos 0)
                                 #f
                               (let ([highlightedSong (list-ref
                                                        masterList
                                                        highlightIndex)])
                                 (list-index
                                   (lambda (song) (equal? song highlightedSong))
                                   newMasterLst)))]
                       [shouldAdjust      (and
                                            newHighlightIndex
                                            (not (between?
                                                   (1- begPos)
                                                   newHighlightIndex
                                                   endPos)))]
                       [heightToPW        (1- (calculate-height))]
                       [tempNewBegPos     (if (not shouldAdjust)
                                              begPos
                                            (pos-or-to-zero
                                              (-
                                                newHighlightIndex
                                                (1- highlightPos))))]
                       [newBegPos         (if (and
                                                (>= lengthOfNML heightToPW)
                                                (>
                                                  (+ tempNewBegPos heightToPW)
                                                  (1- lengthOfNML)))
                                              (- lengthOfNML heightToPW)
                                            tempNewBegPos)]
                       [newHighlightPos   (if (not newHighlightIndex)
                                              0
                                            (1+
                                              (- newHighlightIndex newBegPos)))]
                       [newEndPos         (if (not shouldAdjust)
                                              endPos
                                            (if (> heightToPW (-
                                                                lengthOfNML
                                                                newBegPos))
                                                lengthOfNML
                                              (+ newBegPos heightToPW)))])
                  (clear-lines window heightToPW 1 0)

                  (columned-window
                    window          playWindow                  mpdClient
                    newMasterLst    (rebuild-columns
                                      window
                                      (:
                                        newMasterLst
                                        newBegPos
                                        newEndPos)
                                      allColumns
                                      (playWindow #:get-height)
                                      newSelecDets)             newSelecDets
                    newHighlightPos newBegPos                   newEndPos)))]
        [(#:move-select)
              (when (not (assq-ref selectModeDetails 'status))
                (endwin)
                (error (string-append
                         "In procedure columned-window#:move-select: can't "
                         "move selected columns while not in Selection Mode.")))
              (let* ([moveAmount                             (car xs)]
                     [moveIsNeg                (negative? moveAmount)]
                     [lastIndex              (1- (length allColumns))]
                     [oldIndex    (assq-ref selectModeDetails 'index)]
                     [newIndex                (+ oldIndex moveAmount)]
                     [realNewInd (cond
                                  [(< newIndex 0)                 0]
                                  [(> newIndex lastIndex) lastIndex]
                                  [else                    newIndex])]
                     [playHeight            (playWindow #:get-height)])
                ((list-ref allColumns oldIndex)   #:highlight-column
                                                    playHeight
                                                    #f)
                ((list-ref allColumns realNewInd) #:highlight-column
                                                    playHeight
                                                    #t)
                (columned-window
                  window       playWindow
                  mpdClient    masterList
                  allColumns   (alist 'status      #t
                                      'index       realNewInd
                                      'sortedIndex (assq-ref
                                                     selectModeDetails
                                                     'sortedIndex))
                  highlightPos begPos                                  endPos))]
        [(#:change-select)
             (when (not (assq-ref selectModeDetails 'status))
               (endwin)
               (error (string-append
                        "In procedure columned-window#:change-select: can't "
                        "increase selected column while not in Selection "
                        "Mode.")))
             (columned-window
               window
               playWindow
               mpdClient
               masterList
               (let* ([index         (assq-ref selectModeDetails 'index)]
                      [unalteredCols              (: allColumns 0 index)]
                      [colsToCheck                  (: allColumns index)]
                      [selectedCol                     (car colsToCheck)]
                      [colsAfterSel                    (cdr colsToCheck)]
                      [lastIndex                (1- (length allColumns))]
                      [delta                                    (car xs)])
                 (if (or
                       (= index lastIndex)
                       (and (negative? delta) (>= 3 (selectedCol #:get-width)))
                       (and
                         (positive? delta)
                         (every (lambda (col)
                                  (<= (col #:get-width) 3)) colsAfterSel)))
                     allColumns
                   (let* ([winWidth                                 (cols)]
                          [colsToRight     (-       lastIndex       index)]
                          [initPerc        (/           delta    winWidth)]
                          [otherPercs      (/ (* initPerc -1) colsToRight)]
                          [newSelPerc  (+ (selectedCol
                                            #:get-percentage)    initPerc)]
                          [newSelWidth      (calc-column-width newSelPerc)]
                          [nextOffset  (+ (selectedCol
                                            #:get-offset)     newSelWidth)]
                          [colsToAlter (map
                                         (lambda (col)
                                           (list
                                             (return-if (col #:get-percentage)
                                               (col #:get-width))
                                             #f  ; This'll always be overwritten
                                             col))
                                         colsAfterSel)])
                     (let loop ([finalCols             '()]
                                [remainingCols colsToAlter]
                                [alteration?            #f]
                                [finalOffset    nextOffset]
                                [countdown        initPerc])
                       (cond
                        [(and (null? remainingCols) (or
                                                      (<= countdown 0)
                                                      (not alteration?)))
                              (let* ([playHeight   (playWindow #:get-height)]
                                     [colHeight       (- (lines) playHeight)]
                                     [process-cols
                                           (lambda (colList)
                                             ((caddr colList) #:rebuild-manually
                                                                (car  colList)
                                                                (cadr colList)
                                                                colHeight))])
                                (append
                                  unalteredCols
                                  (let ([col (selectedCol #:rebuild-manually
                                                            newSelPerc
                                                            (selectedCol
                                                              #:get-offset)
                                                            colHeight)])
                                    (col #:highlight-column playHeight #t)
                                    (list col))
                                  (map process-cols (reverse finalCols))
                                  (map process-cols       remainingCols)))]
                        [(and (null? remainingCols) alteration?)
                              (loop '() (reverse finalCols)
                                    #f  nextOffset          countdown)]
                        [(null? (cdr remainingCols))
                              (let* ([curr               (car remainingCols)]
                                     [newWid        (- winWidth finalOffset)]
                                     [alter?                    (> newWid 3)]
                                     [finalWid (if alter? newWid (car curr))])
                                (loop
                                  (cons
                                    (list finalWid finalOffset (caddr curr))
                                    finalCols)
                                  (cdr remainingCols)
                                  (or alteration? alter?)
                                  (+ finalOffset finalWid)
                                  (if alter?
                                      (+ countdown otherPercs)
                                    countdown)))]
                        [else (let* ([curr                 (car remainingCols)]
                                     [newPerc        (+ (car curr) otherPercs)]
                                     [newWid       (calc-column-width newPerc)]  ; It's much slower, though
                                     [alter?                     (>= newWid 3)]  ; (> (calc-column-width (car curr)) 3)
                                     [finalPerc (if alter? newPerc (car curr))])
                                (loop
                                  (cons
                                    (list finalPerc finalOffset (caddr curr))
                                    finalCols)
                                  (cdr remainingCols)
                                  (or alteration? alter?)
                                  (+ finalOffset (calc-column-width finalPerc))
                                  (if alter?
                                      (+ countdown otherPercs)
                                    countdown)))])))))
               selectModeDetails
               highlightPos
               begPos
               endPos)]
        [(#:move-cursor)
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
                         selectModeDetails
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
                                        allColumns   selectModeDetails
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
                         selectModeDetails
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
                           selectModeDetails
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
                           selectModeDetails
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
                           selectModeDetails
                           highlightPos
                           newBegPos
                           newEndPos))]
                 [else (display "Purposeful Error")]))]
        [(#:add-new-line)
              (let* ([line                                     (car xs)]
                     [masterLen                     (length masterList)]
                     [winHeight                      (calculate-height)]
                     [index          (if (cadr xs) (cadr xs) masterLen)]
                     [l?         (and
                                   (>= index begPos)
                                   (< (- index begPos) (1- winHeight)))]
                     [inc?      (< (- masterLen begPos) (1- winHeight))])
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
                            (col #:add-new-line line modPos
                                                inc? (playWindow #:get-height)))
                          allColumns))
                    allColumns)
                  selectModeDetails
                  highlightPos
                  begPos
                  (if inc? (1+ endPos) endPos)))]
        [(#:rebuild)
              (erase window)
              (let* ([pw            (playWindow #:rebuild-size)]
                     [listLen               (length masterList)]
                     [remaining              (- listLen begPos)]
                     [linesHeight       (1- (calculate-height))]
                     [currSongIndx (1- (+ begPos highlightPos))]
                     [winSmaller      (> remaining linesHeight)]
                     [winHalf         (round (/ linesHeight 2))]
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
                (refresh (playWindow #:get-window))
                (columned-window
                  window
                  pw
                  mpdClient
                  masterList
                  (rebuild-columns
                    window
                    (: masterList newBegPos newEndPos)
                    allColumns
                    (pw #:get-height)
                    selectModeDetails)
                  selectModeDetails
                  (if (> (- listLen begPos) linesHeight)
                      (- (1+ currSongIndx) newBegPos)
                    highlightPos)
                  newBegPos
                  newEndPos))])))

  (define* (play-window window runningThread sBox dBox #:optional [setHeight 3])
    (define (prev-status-state=? previousInfo statusToCheckAgainst)
      (string=? (substring (caaadr previousInfo) 0 3) statusToCheckAgainst))

    (define (calc-progress-bar elapsed totalTime isStopped)
      (define (parse-seconds seconds)
        (let* ([rounded        (inexact->exact (round seconds))]
               [mins    (number->string (quotient  rounded 60))]
               [secs    (number->string (remainder rounded 60))])
          (string-append mins ":" (if (= (string-length secs) 1)
                                      (string-append "0" secs)
                                    secs))))

      (let* ([remaining                                 (- totalTime elapsed)]
             [eString                               (parse-seconds   elapsed)]
             [rString                               (parse-seconds remaining)]
             [tString                               (parse-seconds totalTime)]
             [totalLength                       (- (cols)                  3
                                                   (string-length eString) 4
                                                   (string-length rString) 3
                                                   (string-length tString) 1)]
             [firstLength (if (zero? totalTime)
                              0
                            (inexact->exact
                              (floor (* (/ elapsed totalTime) totalLength))))])
        (string-append
          eString
          " ["
          (string-concatenate/shared
            (make-list (pos-or-to-zero firstLength)                   "="))
          (if isStopped "-" ">")
          (string-concatenate/shared
            (make-list (pos-or-to-zero (- totalLength firstLength 1)) "-"))
          "] "
          rString
          " / "
          tString)))

    (define (write-line wind lineIndex linesToWrite rev?)
      (define funct (if rev? inverse-on inverse-off))

      (let ([x (fold
                 (lambda (line result)
                   (addchstr wind          (funct ((cdr line) (car line)))
                             #:y lineIndex #:x result)

                   (+ result (string-length (car line))))
                 0
                 linesToWrite)])
        (addchstr
          wind
          (funct (string-concatenate/shared
                   (make-list (pos-or-to-zero (- (cols) x)) " ")))
          #:y lineIndex
          #:x x)))

    (define (set-display! win mpdClient statusBox displayedSongBox)
      (mpd-connect mpdClient)
      (let ([status        (get-mpd-response (mpdStatus::status mpdClient))]
            [startingIndex                            (- (lines) setHeight)])
        (mpd-disconnect mpdClient)

        (case (string->symbol (assoc-ref status 'state))
          [(stop)  (let ([winWidth                     (cols)]
                         [prevInfo (atomic-box-ref statusBox)]
                         [dispSong    (list (cons "" normal))])
                     (write-line win startingIndex dispSong #t)
                     (when (or
                             (not prevInfo)
                             (not (and
                                    (= winWidth (car prevInfo))
                                    (prev-status-state=? prevInfo " ▪ "))))
                       (let ([newStatus (list (cons
                                                (string-append
                                                  " ▪ "
                                                  (calc-progress-bar 0 0 #t))
                                                normal))])
                         (write-line win (1+ startingIndex) newStatus #t)
                         (atomic-box-set! statusBox (list
                                                      winWidth
                                                      newStatus
                                                      (cons 0 0)))))
                     (atomic-box-set! displayedSongBox dispSong))]
          [(play)  (mpd-connect mpdClient)
                   (let ([song (get-mpd-response
                                 (mpdStatus::current-song mpdClient))])
                     (mpd-disconnect mpdClient)
                     (let* ([elapsed   (assoc-ref status 'elapsed)]
                            [time      (assoc-ref song   'Time)]
                            [newStatus (list (cons (string-append
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
                       (write-line win startingIndex      dispSong  #t)
                       (write-line win (1+ startingIndex) newStatus #t)

                       (atomic-box-set! statusBox        (list
                                                           (cols)
                                                           newStatus
                                                           (cons elapsed time)))
                       (atomic-box-set! displayedSongBox dispSong)))]
          [(pause) (let ([winWidth                     (cols)]
                         [prevInfo (atomic-box-ref statusBox)])
                     (write-line win startingIndex (atomic-box-ref
                                                     displayedSongBox) #t)

                     (if (= winWidth (car prevInfo))
                         (when (not (prev-status-state=? prevInfo " 𝍪 "))
                           (let ([newStatus (list (cons
                                                    (string-append
                                                     " 𝍪 "
                                                     (substring (caaadr
                                                                  prevInfo) 3))
                                                    (cdaadr prevInfo)))])
                             (write-line win (1+ startingIndex) newStatus #t)
                             (atomic-box-set! statusBox (list
                                                          winWidth
                                                          newStatus
                                                          (caddr prevInfo)))))
                       (let* ([prevTimes (caddr prevInfo)]
                              [newStatus (list (cons (string-append
                                                       " 𝍪 "
                                                       (calc-progress-bar
                                                        (car prevTimes)
                                                        (cdr prevTimes)
                                                        #f)) normal))])
                         (write-line win (1+ startingIndex) newStatus #t)
                         (atomic-box-set! statusBox (list
                                                      winWidth
                                                      newStatus
                                                      prevTimes)))))])
        (refresh win))
      (sleep 1)
      (set-display! win mpdClient statusBox displayedSongBox))

    (let ([thread (if runningThread
                      runningThread
                    (call-with-new-thread
                      (lambda ()
                        (set-display! window (new-mpd-client) sBox dBox))))])
      (lambda (method . xs)
        (case method
          [(#:get-window)                     window]
          [(#:get-height)                  setHeight]
          [(#:rebuild-play)
               (let ([prevInfo      (atomic-box-ref sBox)]
                     [startingIndex (- (lines) setHeight)])
                 (write-line window startingIndex (atomic-box-ref dBox) #t)
                 (write-line
                   window
                   (1+ startingIndex)
                   (list (cons
                           (string-append " ▶" (substring (caaadr prevInfo) 2))
                           (cdaadr prevInfo)))
                   #t))
               (play-window window thread sBox dBox)]
          [(#:rebuild-pause)
               (let* ([startingIndex (- (lines) setHeight)]
                      [prevInfo      (atomic-box-ref sBox)]
                      [state             (caaadr prevInfo)]
                      [sym           (substring state 0 2)])
                 (write-line window startingIndex (atomic-box-ref dBox) #t)
                 (write-line
                   window
                   (1+ startingIndex)
                   (list (cons
                           (string-append
                             (cond
                              [(string=? sym " 𝍪") " ▶"]
                              [(string=? sym " ▶") " 𝍪"]
                              [else                " ▪"])
                             (substring state 2))         (cdaadr prevInfo)))
                   #t))
               (play-window window thread sBox dBox)]
          [(#:rebuild-size)
               (let* ([initIndex (- (lines) setHeight)]
                      [prevInfo  (atomic-box-ref sBox)]
                      [prevTimes      (caddr prevInfo)]
                      [newStatus (list
                                   (cons
                                     (string-append
                                       (substring (caaadr prevInfo) 0 3)
                                       (calc-progress-bar
                                         (car prevTimes)
                                         (cdr prevTimes)
                                         (prev-status-state=? prevInfo " ▪ ")))
                                     (cdaadr prevInfo)))])
                 (write-line window initIndex      (atomic-box-ref dBox) #t)
                 (write-line window (1+ initIndex) newStatus             #t)
                 (atomic-box-set! sBox (list (cols) newStatus prevTimes)))
               (play-window window thread sBox dBox)]))))



  (columned-window
    stdscr
    (play-window   stdscr #f (make-atomic-box #f) (make-atomic-box #f))
    mpd
    '()
    (build-columns stdscr captions)
    (alist 'status #f 'index #f 'sortedIndex #f)
    0
    0
    0))
