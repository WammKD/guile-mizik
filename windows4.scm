#!/usr/local/bin/guile
!#
(use-modules (srfi srfi-1) (ice-9 threads) (ice-9 atomic))
(include     "./util.scm")

(define (windows::build-columned-window stdscr       mainWindow
                                        bottomWindow mpd        . captions)
  (define (build-columns window headers)
    (let ([windowWidth          (getmaxx window)]
          [totalLenOfEachHeader (fold
                                  (lambda (header total)
                                    (+ (string-length (car header)) total))
                                  0
                                  headers)])
      (let loop ([result               '()]
                 [remainingHeaders headers]
                 [offset                 0])
        (let ([header (car remainingHeaders)])
          (if (null? (cdr remainingHeaders))
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
                (cdr remainingHeaders)
                (+ (col #:get-width) offset))))))))
  (define (rebuild-columns window lines currentColumns playWindowHeight)
    (let loop ([result                      '()]
               [remainingColumns currentColumns]
               [offset                        0])
      (if (null? (cdr remainingColumns))
          (reverse (cons ((car remainingColumns) #:rebuild-with-size
                                                   lines
                                                   offset
                                                   (- (getmaxx window) offset)
                                                   playWindowHeight) result))
        (let ([col ((car remainingColumns)
                     #:rebuild-with-size lines offset #f playWindowHeight)])
          (loop
            (cons col result)
            (cdr remainingColumns)
            (+ (col #:get-width) offset))))))

  (define* (column window format-line
                   header lines
                   offset percentage  #:optional [formattedLines #f]
                                                 [setHeader      #f]
                                                 [setLines       #f])
    (define body        (if formattedLines
                            formattedLines
                          (map
                            (lambda (line)
                              (format-line
                                (assoc-ref line (string->symbol header))))
                            lines)))
    (define columnWidth (if (car percentage)
                            (round (* (cdr percentage) (getmaxx window)))
                          (cdr percentage)))



    (define (check-height newLinesLength playWindowHeight)
      (let ([linesLength (1- (getmaxy window))])
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



    (let ([newHeader (if setHeader setHeader (format-and-add header 0))]
          [newLines  (if setLines
                         setLines
                       (map format-and-add body (iota (length body) 1)))]
          [linesLen  (length lines)])
      (lambda (method . xs)
        (case method
          [(#:get-width)                           columnWidth]
          [(#:get-offset)                               offset]
          [(#:get-tag)                 (symbol->string header)]
          [(#:get-header)                               header]
          [(#:get-formed-header)                     newHeader]
          [(#:get-lines)                                 lines]
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
                 (iota (- (getmaxy window) (car xs))))]
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
          [(#:rebuild)                                 (column
                                                         window
                                                         format-line
                                                         header
                                                         (car xs)
                                                         offset
                                                         percentage)]
          [(#:rebuild-with-size)
                (check-height (length (car xs)) (cadddr xs))

                (column window   format-line header
                        (car xs) (cadr xs)   (let ([perc (caddr xs)])
                                               (if perc
                                                   (cons #f perc)
                                                 percentage)))]
          [(#:rebuild-manually)
               (let ([perc (car xs)] [offSet (cadr xs)] [clrHt (caddr xs)])
                 (clear-lines window (if clrHt
                                         clrHt
                                       (1+ (length lines))) 0 offSet)

                 (column
                   window
                   format-line
                   header
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
    (define (calculate-height)
      (- (lines) (playWindow #:get-expected-height)))

    (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
    (if (>= highlightPos (calculate-height))
        (error highlightPos)
      (when (not (car isInSelectionMode))
        (chgat window -1 A_REVERSE 0 #:x 0 #:y highlightPos)))

    (refresh window)

    (lambda (method . xs)
      (case method
        [(#:get-window)                    window]
        [(#:get-max-y-x)        (getmaxyx window)]
        [(#:get-max-y)          (getmaxy  window)]
        [(#:get-max-x)          (getmaxx  window)]
        [(#:is-in-mode)   (car isInSelectionMode)]
        [(#:refresh)            (refresh  window)]
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
                          (columned-window window       playWindow mpdClient
                                           masterList   allColumns (cons #t 0)
                                           highlightPos begPos     endPos)]
        [(#:rebuild)
              (erase window)
              (let ([winHeight (calculate-height)])
                (mvwin  window 0         0)
                (resize window winHeight (cols))
                (let* ([pw            (playWindow #:rebuild-size)]
                       [listLen               (length masterList)]
                       [remaining              (- listLen begPos)]
                       [linesHeight                (1- winHeight)]
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
                      (pw #:get-height))
                    isInSelectionMode
                    (if (> (- listLen begPos) linesHeight)
                        (- (1+ currSongIndx) newBegPos)
                      highlightPos)
                    newBegPos
                    newEndPos)))])))

  (define* (play-window window runningThread sBox dBox #:optional [setHeight 3])
    (define (prev-status-state=? previousInfo statusToCheckAgainst)
      (string=? (substring (caaadr previousInfo) 0 3) statusToCheckAgainst))

    (define (calc-progress-bar elapsed totalTime stopped?)
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
             [totalLength                       (- (getmaxx window)        3
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
          (if stopped? "-" ">")
          (string-concatenate/shared
            (make-list (pos-or-to-zero (- totalLength firstLength 1)) "-"))
          "] "
          rString
          " / "
          tString)))

    (define (write-line wind lineIndex lines rev?)
      (let* ([funct (if rev? inverse-on inverse-off)]
             [x     (fold
                      (lambda (line result)
                        (addchstr wind          (funct
                                                  ((cdr line) (car line)))
                                  #:y lineIndex #:x result)

                        (+ result (string-length (car line))))
                      0
                      lines)])
        (addchstr
          wind
          (funct
            (string-concatenate/shared
              (make-list (pos-or-to-zero (- (getmaxx wind) x)) " ")))
          #:y lineIndex
          #:x x)))

    (define (set-display! win mpdClient statusBox displayedSongBox)
      (mpd-connect mpdClient)
      (let ([status (get-mpd-response (mpdStatus::status mpdClient))])
        (mpd-disconnect mpdClient)

        (case (string->symbol (assoc-ref status 'state))
          [(stop)  (let ([winWidth              (getmaxx win)]
                         [prevInfo (atomic-box-ref statusBox)]
                         [dispSong    (list (cons "" normal))])
                     (write-line win 0 dispSong #t)
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
                         (write-line win 1 newStatus #t)
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
                       (write-line win 0 dispSong  #t)
                       (write-line win 1 newStatus #t)

                       (atomic-box-set! statusBox        (list
                                                           (getmaxx win)
                                                           newStatus
                                                           (cons elapsed time)))
                       (atomic-box-set! displayedSongBox dispSong)))]
          [(pause) (let ([winWidth              (getmaxx win)]
                         [prevInfo (atomic-box-ref statusBox)])
                     (write-line win 0 (atomic-box-ref displayedSongBox) #t)

                     (if (= winWidth (car prevInfo))
                         (when (not (prev-status-state=? prevInfo " 𝍪 "))
                           (let ([newStatus (list (cons
                                                    (string-append
                                                     " 𝍪 "
                                                     (substring (caaadr
                                                                  prevInfo) 3))
                                                    (cdaadr prevInfo)))])
                             (write-line win 1 newStatus #t)
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
                         (write-line win 1 newStatus #t)
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
          [(#:get-window)                    window]
          [(#:get-height)          (getmaxy window)]
          [(#:get-expected-height)        setHeight]
          [(#:rebuild-play)
               (let ([prevInfo (atomic-box-ref sBox)])
                 (write-line window 0 (atomic-box-ref dBox) #t)
                 (write-line
                   window
                   1
                   (list (cons
                           (string-append " ▶" (substring (caaadr prevInfo) 2))
                           (cdaadr prevInfo)))
                   #t))
               (play-window window thread sBox dBox)]
          [(#:rebuild-pause)
               (let* ([prevInfo (atomic-box-ref sBox)]
                      [state        (caaadr prevInfo)]
                      [sym      (substring state 0 2)])
                 (write-line window 0 (atomic-box-ref dBox) #t)
                 (write-line
                   window
                   1
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
               (mvwin  window (- (lines) setHeight) 0)
               (resize window setHeight             (cols))
               (let* ([prevInfo  (atomic-box-ref sBox)]
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
                 (write-line window 0 (atomic-box-ref dBox) #t)
                 (write-line window 1 newStatus             #t)
                 (atomic-box-set!
                   sBox
                   (list (getmaxx window) newStatus prevTimes)))
               (refresh window)
               (play-window window thread sBox dBox)]))))



  (when (not (= (getmaxy bottomWindow) 3))
    (error (string-append/shared
             "In procedure windows::build-columned-window: the bottom-bar "
             "window isn't the correct height of 3; it's "
             (number->string (getmaxy bottomWindow)))))

  (columned-window
    mainWindow
    (play-window   bottomWindow #f (make-atomic-box #f) (make-atomic-box #f))
    mpd
    '()
    (build-columns mainWindow captions)
    (cons #f #f)
    0
    0
    0))
