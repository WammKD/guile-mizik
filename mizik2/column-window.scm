#!/usr/bin/guile
!#
(use-modules (srfi srfi-1) (srfi srfi-9))
(include     "./columns.scm")
(include     "./select-mode-status.scm")
(include     "./util.scm")

(define (calculate-columned-height)
  (- (lines) PLAY_WINDOW_HEIGHT))

(define-record-type <mizik-column-window>
  (make-mizik-column-window window            mpdClient
                            songList          columns
                            selectModeDetails highlightPosition)
  mizik-column-window?
  (window            curses-window       curses-window-set!)
  (mpdClient         mpd-client          mpd-client-set!)
  (songList          song-list           song-list-set!)
  (columns           columns             columns-set!)
  (selectModeDetails select-mode-details select-mode-details-set!)
  (highlightPosition position-highlight  position-highlight-set!))

(define (generate-column-window client rawColumns)
  (let* ([winHeight (calculate-columned-height)]
         [songCount (assq-ref
                      (get-mpd-response (mpdStatus::status client))
                      'playlistlength)]
         [w         (make-mizik-column-window
                      (newwin winHeight (cols) 0 0)
                      client
                      (get-mpd-response (mpdPlaylistCurrent::playlist-info
                                          client
                                          0
                                          (if (> songCount (1- winHeight))
                                              (1- winHeight)
                                            songCount)))
                      (generate-columns rawColumns)
                      (initialize-select-mode-status)
                      0)])
    (render-column-window w)

    w))

(define (move-cursor columnedWindow moveDegree)
  (define client       (mpd-client columnedWindow))
  (define songCount    (begin
                         (mpd-connect client)

                         (assq-ref
                           (get-mpd-response (mpdStatus::status client))
                           'playlistlength)))
  (define winLen       (getmaxy (curses-window columnedWindow)))
  (define highlightPos (position-highlight columnedWindow))
  (define newHghlghPos (+ highlightPos moveDegree))
  (define       begPos (if (zero? songCount) #f (assq-ref
                                                  (car (song-list columnedWindow))
                                                  'Pos)))
  (define    newBegPos (if (zero? songCount) #f (+ begPos      moveDegree)))
  (define       endPos (if (zero? songCount) #f (assq-ref
                                                  (car (reverse
                                                         (song-list
                                                           columnedWindow)))
                                                  'Pos)))
  (define    newEndPos (if (zero? songCount) #f (+ (1+ endPos) moveDegree)))
  (define inSongLstPos (if (zero? songCount) #f (+ begPos (1- highlightPos))))
  (define newSngLstPos (if (zero? songCount) #f (+ begPos (1- newHghlghPos))))

  (cond
   [(or
      ;; Empty Playlist
      (= songCount 0)
      (and (= highlightPos 0) (negative? moveDegree)))
         ]
   ;; trying to move above position 0 in playlist
   [(and (> 1 newHghlghPos) (> 0 newBegPos) (< -1 (+ inSongLstPos moveDegree)))
         (song-list-set! columnedWindow (get-mpd-response
                                          (mpdPlaylistCurrent::playlist-info
                                            client
                                            0
                                            (1- winLen))))

         (position-highlight-set! columnedWindow (1+ newSngLstPos))]
   ;;
   [(and (> 1 newHghlghPos) (> 0 newBegPos) (> 0 (+ inSongLstPos moveDegree)))
         (song-list-set! columnedWindow (get-mpd-response
                                          (mpdPlaylistCurrent::playlist-info
                                            client
                                            0
                                            (1- winLen))))

         (position-highlight-set! columnedWindow 1)]
   ;; trying to move below last song in playlist
   [(and (<= winLen newHghlghPos) (<= songCount newSngLstPos))
         (song-list-set! columnedWindow (get-mpd-response
                                          (mpdPlaylistCurrent::playlist-info
                                            client
                                            (- songCount (1- winLen))
                                            songCount)))

         (position-highlight-set! columnedWindow (1- winLen))]
   ;; moving below last visible song such that the new list of songs
   ;; for the next page are less than the length of the page but the
   ;; new cursor position is on a song that's not the very last song
   [(< (- songCount newBegPos) (2- winLen))
         (song-list-set! columnedWindow (get-mpd-response
                                          (mpdPlaylistCurrent::playlist-info
                                            client
                                            (- songCount (1- winLen))
                                            songCount)))

         (position-highlight-set! columnedWindow (-
                                                   winLen
                                                   (- songCount newSngLstPos)))]
   [(or (> 1 newHghlghPos) (<= winLen newHghlghPos))
         (song-list-set! columnedWindow (get-mpd-response
                                          (mpdPlaylistCurrent::playlist-info
                                            client
                                            newBegPos
                                            newEndPos)))]
   [(< 0 newHghlghPos winLen)
         (position-highlight-set! columnedWindow newHghlghPos)])

  (mpd-disconnect client)

  (render-column-window columnedWindow)

  columnedWindow)

(define (rebuild stdScr columnedWindow)
  (define newWinHeight (calculate-columned-height))
  (define window       (curses-window      columnedWindow))
  (define highlightPos (position-highlight columnedWindow))
  (define       begPos (assq-ref (car (song-list columnedWindow)) 'Pos))
  (define inSongLstPos (+ begPos (1- highlightPos)))
  (define newFirstHalf (floor (/ newWinHeight 2)))
  (define       client (mpd-client columnedWindow))

  (clear   stdScr)
  (refresh stdScr)
  (clear   window)
  (resize  window newWinHeight (cols))

  (mpd-connect client)
  (let* ([songCount    (assq-ref
                         (get-mpd-response (mpdStatus::status client))
                         'playlistlength)]
         [decider      (let ([testBeg (- (1+ inSongLstPos) newFirstHalf)])
                         (cond
                          [(<  testBeg                               0)  1]
                          [(>= (+ testBeg (1- newWinHeight)) songCount)  0]
                          [else                                         -1]))]
         [newBegPos    (case decider
                         [( 1)                                  0]
                         [( 0)    (- songCount (1- newWinHeight))]
                         [(-1) (- (1+ inSongLstPos) newFirstHalf)])]
         [newHghlghPos (case decider
                         [( 1)                           (1+ inSongLstPos)]
                         [( 0) (- newWinHeight (- songCount inSongLstPos))]
                         [(-1)                                newFirstHalf])])
    (position-highlight-set! columnedWindow newHghlghPos)

    (mpd-disconnect client)
    (mpd-connect client)

    (song-list-set! columnedWindow (get-mpd-response
                                     (mpdPlaylistCurrent::playlist-info
                                       client
                                       newBegPos
                                       (+ newBegPos (1- newWinHeight)))))

    (mpd-disconnect client))

  (render-column-window columnedWindow)

  columnedWindow)

(define (render-column-window columnedWindow)
  (define window       (curses-window      columnedWindow))
  (define highlightPos (position-highlight columnedWindow))

  (let render-rows ([rows     (cons
                                (map
                                  (lambda (column)
                                    (let ([theTitle (title column)])
                                      (cons (string->symbol theTitle) theTitle)))
                                  (columns columnedWindow))
                                (song-list columnedWindow))]
                    [isHeader #t]
                    [index    0])
    (addstr
      window
      (let process-cols ([theCols (columns columnedWindow)]
                         [rowStr                        ""]
                         [offset                         0])
        (if (null? theCols)
            rowStr
          (let* ([column         (car theCols)]
                 [refinedColPart ((if isHeader identity (formatter column))
                                   (assq-ref (car rows) (string->symbol
                                                          (title column))))]
                 [finalColPart   (if refinedColPart refinedColPart "")]
                 [colPartLen     (string-length finalColPart)]
                 [columnWidth    (if (null? (cdr theCols))
                                     (- (cols) offset)
                                   (round (* (cols) (percentage column))))]
                 [colPartEnd     (if isHeader (case-pred (column-sorted? column)
                                                [negative? " "]
                                                [zero?     "▼"]
                                                [positive? "▲"]) " ")])
            (process-cols
              (cdr theCols)
              (string-append
                rowStr
                (cond
                 [(<= columnWidth 0)                                   ""]
                 [(=  columnWidth 1)                             ELLIPSIS]
                 [(=  columnWidth 2)  (string-append ELLIPSIS colPartEnd)]
                 [(>
                    colPartLen
                    (1- columnWidth)) (string-append
                                        (substring finalColPart 0 (2- columnWidth))
                                        ELLIPSIS
                                        colPartEnd)]
                 [(=
                    colPartLen
                    (1- columnWidth)) (string-append finalColPart colPartEnd)]
                 [(=
                    colPartLen
                    columnWidth)      (string-append finalColPart " " colPartEnd)]
                 [else                (string-append
                                        finalColPart
                                        (make-string (-
                                                       columnWidth
                                                       (string-length finalColPart)
                                                       2) #\space)
                                        colPartEnd
                                        " ")]))
              (+ offset columnWidth)))))
      #:x 0
      #:y index)

    (when (not (null? (cdr rows)))
      (render-rows (cdr rows) #f (1+ index))))

  (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
  (if (>= highlightPos (getmaxy window))
      (begin (endwin) (error highlightPos))
    (when (not (active? (select-mode-details columnedWindow)))
      (chgat window -1 A_REVERSE 0 #:x 0 #:y highlightPos)))

  (refresh window))

(define (clear-lines-of-columns columnedWindow startingVertIndex)
  (let ([win (curses-window columnedWindow)])
    (move win startingVertIndex 0)

    (clrtobot win)))
