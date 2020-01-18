#!/usr/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./column-window.scm")

(setlocale LC_ALL "")

(define (main-loop standardScreen columnedWin)
  (define char (getch standardScreen))

  (when (not (equal? char #\q))
    (main-loop
      standardScreen
      (if (equal? char KEY_RESIZE)
          (retry-on-fail (rebuild standardScreen columnedWin))
        (if (active? (select-mode-details columnedWin))
            (case char  ; Stupid case doesn't recognize variables
              [(#\s)                 (columnedWin #:sort-select)]
              [else                  columnedWin])
              [(260) #| KEY_LEFT  |# (move-select-mode     columnedWin -1)]
              [(261) #| KEY_RIGHT |# (move-select-mode     columnedWin  1)]
              [(#\=)                 (change-column-size   columnedWin  1)]
              [(#\-) #| KEY_ENTER |# (change-column-size   columnedWin -1)]
              [(339) #| KEY_PPAGE |# (move-column-position columnedWin -1)]
              [(338) #| KEY_NPAGE |# (move-column-position columnedWin  1)]
              [(13 10 343 #\newline)       (leave-select-mode columnedWin)]
          (case char
            [(338) #| KEY_NPAGE  |# (move-cursor columnedWin   10 )]
            [(#\n)                  (move-cursor columnedWin    3 )]
            [(258) #| KEY_DOWN   |# (move-cursor columnedWin    1 )]
            [(339) #| KEY_PPAGE  |# (move-cursor columnedWin  -10 )]
            [(#\p)                  (move-cursor columnedWin   -3 )]
            [(259) #| KEY_UP     |# (move-cursor columnedWin   -1 )]
            [(402) #| KEY_SRIGHT |# (seek        columnedWin  "+5")]
            [(393) #| KEY_SLEFT  |# (seek        columnedWin  "-5")]
            [(261) #| KEY_RIGHT  |# (seek        columnedWin "+10")]
            [(260) #| KEY_LEFT   |# (seek        columnedWin "-10")]
            [(13
              10
              343  ; KEY_ENTER
              #\newline)               (play        columnedWin)]
            [(#\space)                 (toggle-play columnedWin)]
            [(#\v)                   (set-volume columnedWin  5)]
            [(#\V)                   (set-volume columnedWin -5)]
            [(#\L)                   (toggle-repeat columnedWin)]
            [(#\R)                   (toggle-random columnedWin)]
            [(#\s)               (enter-select-mode columnedWin)]
            [(#\esc)     (nodelay! standardScreen #t)
                         (let* ([w standardScreen]
                                [c      (getch w)])
                           (nodelay! w #f)

                           ;; If false, the user just pressed ESC
                           (if c
                               (begin
                                 (when (and
                                         (equal? c         #\[)
                                         (equal? (getch w) #\1)
                                         (equal? (getch w) #\;))
                                   (let ([newC (getch w)])
                                     (case newC
                                       ;; Shift
                                       [(#\2) (let ([nnC (getch w)])
                                                (case nnC
                                                  [(#\C) (seek columnedWin "+5")]
                                                  [(#\D) (seek columnedWin "-5")]))]
                                       ;; Alt
                                       [(#\3) #t]
                                       ;; Alt + Shift
                                       [(#\4) #t]
                                       ;; Ctrl
                                       [(#\5) (let ([nnC (getch w)])
                                                (case nnC
                                                  [(#\C) (seek columnedWin "+60")]
                                                  [(#\D) (seek columnedWin "-60")]))]
                                       ;; Ctrl + Shift
                                       [(#\6) (let ([nnC (getch w)])
                                                (case nnC
                                                  [(#\C) (seek columnedWin "+10")]
                                                  [(#\D) (seek columnedWin "-10")]))]
                                       ;; Ctrl + Alt
                                       [(#\7) #t]
                                       ;; Ctrl + Shift + Alt
                                       [(#\8) #t])))

                                 columnedWin)
                             ;; Escape key, only
                             columnedWin))]
            [else                                    columnedWin]))))))

;; Initialize the main screen and settings
(define stdscr (initscr))

;; Initialize the MPD client
(define client (new-mpd-client))

(use-default-colors)
(noecho!)
(cbreak!)
(start-color!)
(keypad! stdscr #t)

(refresh stdscr)



(mpd-connect client)

(mpdPlaybackOption::consume! client #f)
(mpdPlaylistCurrent::clear!  client)
(mpdPlaylistCurrent::add!    client (assoc-ref
                                      (car (get-mpd-response
                                             (mpdDatabase::list-all client)))
                                      'directory))

(main-loop stdscr (generate-column-window
                    client
                    `(("Track"  . ,(lambda (track)
                                     (if (not (number? track))
                                         #f
                                       (if-let* ([strT   (number->string track)]
                                                 [index (string-index strT #\/)])
                                           (substring strT 0 index)
                                         strT))))
                      ("Title"  . ,(lambda (title)   title))
                      ("Artist" . ,(lambda (artist) artist))
                      ("Album"  . ,(lambda (album)   album))
                      ("Time"   . ,(lambda (time)
                                     (let* ([r (inexact->exact     (round time))]
                                            [m (number->string (quotient  r 60))]
                                            [s (number->string (remainder r 60))])
                                       (string-append
                                         m
                                         ":"
                                         (if (= (string-length s) 1)
                                             (string-append "0" s)
                                           s)))))
                      ("Genre"  . ,(lambda (genre) genre)))))



(endwin)



(newline)
(newline)
;; (mpd-connect    client)
;; ;; (display (get-mpd-response (mpdDatabase::list-all client)))
;; ;; (display (get-mpd-response (mpdPlaylistCurrent::playlist-info client)))
;; (display (get-mpd-response (mpdStatus::status client)))
;; (mpd-disconnect client)
