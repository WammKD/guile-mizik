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
              [(260) #| KEY_LEFT  |# (columnedWin   #:move-select -1)]
              [(261) #| KEY_RIGHT |# (columnedWin   #:move-select  1)]
              [(#\=)                 (columnedWin #:change-select  1)]
              [(#\-) #| KEY_ENTER |# (columnedWin #:change-select -1)]
              [(339) #| KEY_PPAGE |# (columnedWin   #:move-column -1)]
              [(338) #| KEY_NPAGE |# (columnedWin   #:move-column  1)]
              [(13 10 343 #\newline) (columnedWin #:leave-select)]
              [(#\s)                 (columnedWin #:sort-select)]
              [else                  columnedWin])
          (case char
            [(338) #| KEY_NPAGE |# (move-cursor columnedWin  10)]
            [(#\n)                 (move-cursor columnedWin   3)]
            [(258) #| KEY_DOWN  |# (move-cursor columnedWin   1)]
            [(339) #| KEY_PPAGE |# (move-cursor columnedWin -10)]
            [(#\p)                 (move-cursor columnedWin  -3)]
            [(259) #| KEY_UP    |# (move-cursor columnedWin  -1)]
            [(13
              10
              343  ; KEY_ENTER
              #\newline) (columnedWin #:play)          columnedWin]
            [(#\space)   (columnedWin #:toggle-play)   columnedWin]
            [(#\v)       (columnedWin #:set-vol 5 #t)  columnedWin]
            [(#\V)       (columnedWin #:set-vol 5 #f)  columnedWin]
            [(#\L)       (columnedWin #:toggle-repeat) columnedWin]
            [(#\R)       (columnedWin #:toggle-random) columnedWin]
            [(#\s)                    (columnedWin #:enter-select)]
            [(#\esc)     (nodelay! (columnedWin #:get-window) #t)
                         (let* ([w (columnedWin #:get-window)]
                                [c                  (getch w)])
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
                                                  [(#\C) (columnedWin
                                                           #:seek "+5")]
                                                  [(#\D) (columnedWin
                                                           #:seek "-5")]))]
                                       ;; Alt
                                       [(#\3) #t]
                                       ;; Alt + Shift
                                       [(#\4) #t]
                                       ;; Ctrl
                                       [(#\5) (let ([nnC (getch w)])
                                                (case nnC
                                                  [(#\C) (columnedWin
                                                           #:seek "+60")]
                                                  [(#\D) (columnedWin
                                                           #:seek "-60")]))]
                                       ;; Ctrl + Shift
                                       [(#\6) (let ([nnC (getch w)])
                                                (case nnC
                                                  [(#\C) (columnedWin
                                                           #:seek "+10")]
                                                  [(#\D) (columnedWin
                                                           #:seek "-10")]))]
                                       ;; Ctrl + Alt
                                       [(#\7) #t]
                                       ;; Ctrl + Shift + Alt
                                       [(#\8) #t])))

                                 columnedWin)
                             ;; Escape key, only
                             columnedWin))]
            [else                                     columnedWin]))))))

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
