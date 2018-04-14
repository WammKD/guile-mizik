#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./windows5.scm")

(setlocale LC_ALL "")

(define (main-loop standardScreen columnedWin)
  (define char (getch standardScreen))

  (when (not (equal? char #\q))
    (main-loop
      standardScreen
      (if (equal? char KEY_RESIZE)
          (columnedWin #:rebuild)
        (if (columnedWin #:is-in-mode)
            (case char  ; Stupid case doesn't recognize variables
              [(260) #| KEY_LEFT  |# (columnedWin   #:move-select -1)]
              [(261) #| KEY_RIGHT |# (columnedWin   #:move-select  1)]
              [(#\=)                 (columnedWin #:change-select  1)]
              [(#\-) #| KEY_ENTER |# (columnedWin #:change-select -1)]
              [(13 10 343 #\newline) (columnedWin #:leave-select)]
              [(#\s)                 (columnedWin #:sort-select)]
              [else                  columnedWin])
          (case char
            [(338)#| KEY_NPAGE |# (columnedWin #:move-cursor  10)]
            [(#\n)                (columnedWin #:move-cursor   3)]
            [(258)#| KEY_DOWN  |# (columnedWin #:move-cursor   1)]
            [(339)#| KEY_PPAGE |# (columnedWin #:move-cursor -10)]
            [(#\p)                (columnedWin #:move-cursor  -3)]
            [(259)#| KEY_UP    |# (columnedWin #:move-cursor  -1)]
            [(13
              10
              343  ; KEY_ENTER
              #\newline) (columnedWin #:play)         columnedWin]
            [(#\space)   (columnedWin #:toggle-play)  columnedWin]
            [(#\v)       (columnedWin #:set-vol 5 #t) columnedWin]
            [(#\V)       (columnedWin #:set-vol 5 #f) columnedWin]
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
            [(#\s)                   (columnedWin #:enter-select)]
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

(main-loop
  stdscr
  (let loop ([w (windows::build-columned-window
                  stdscr
                  (new-mpd-client)
                  (cons "Track"  (lambda (track)
                                   (let ([strT (number->string track)])
                                     (if-let ([index (string-index strT #\/)])
                                         (substring strT 0 index)
                                       strT))))
                  (cons "Title"  (lambda (title)   title))
                  (cons "Artist" (lambda (artist) artist))
                  (cons "Album"  (lambda (album)   album))
                  (cons "Time"   (lambda (time)
                                   (let* ([r (inexact->exact     (round time))]
                                          [m (number->string (quotient  r 60))]
                                          [s (number->string (remainder r 60))])
                                     (string-append
                                       m
                                       ":"
                                       (if (= (string-length s) 1)
                                           (string-append "0" s)
                                         s)))))
                  (cons "Genre"  (lambda (genre) genre)))]
             [s (get-mpd-response (mpdPlaylistCurrent::playlist-info client))])
    (if (null? s)
        (begin (mpd-disconnect client) w)
      (loop
        (w #:add-new-line
           (car s)
           #f)
        (cdr s)))))



;; (let main ([mainWindow     (begin
;;                              (mpd-connect client)

;;                              (mpdPlaybackOption::consume! client #f)

;;                              (mpdPlaylistCurrent::clear! client)
;;                              (mpdPlaylistCurrent::add!
;;                                client
;;                                (assoc-ref
;;                                  (car (get-mpd-response
;;                                         (mpdDatabase::list-all client)))
;;                                  'directory))

;;                              (let loop ([w (windows::build-columned-window
;;                                              stdscr
;;                                              (new-mpd-client)
;;                                              (cons
;;                                                "Track"
;;                                                (lambda (track)
;;                                                  (let ([index (string-index
;;                                                                 track
;;                                                                 #\/)])
;;                                                    (if index
;;                                                        (substring
;;                                                          track
;;                                                          0
;;                                                          index)
;;                                                      track))))
;;                                              (cons "Title"  (lambda (title)
;;                                                               title))
;;                                              (cons "Artist" (lambda (artist)
;;                                                               artist))
;;                                              (cons "Album"  (lambda (album)
;;                                                               album))
;;                                              (cons
;;                                                "Time"
;;                                                (lambda (time)
;;                                                  (let* ([r (inexact->exact
;;                                                              (round
;;                                                                (string->number
;;                                                                  time)))]
;;                                                         [m (number->string
;;                                                              (quotient r 60))]
;;                                                         [s (number->string
;;                                                              (remainder
;;                                                                r
;;                                                                60))])
;;                                                    (string-append
;;                                                      m
;;                                                      ":"
;;                                                      (if (= (string-length
;;                                                               s) 1)
;;                                                          (string-append "0" s)
;;                                                        s)))))
;;                                              (cons "Genre"  (lambda (genre)
;;                                                               genre)))]
;;                                         [s (get-mpd-response
;;                                              (mpdPlaylistCurrent::playlist-info
;;                                                client))])
;;                                (if (null? s)
;;                                    (begin
;;                                      (mpd-disconnect client)
;;                                      w)
;;                                  (loop
;;                                    (w #:add-new-line
;;                                         (map
;;                                           (lambda (x)
;;                                             (if (number? (cdr x))
;;                                                 (cons
;;                                                   (car x)
;;                                                   (number->string (cdr x)))
;;                                               x))
;;                                           (car s))
;;                                         #f)
;;                                    (cdr s)))))]
;;            [pastDimensions (getmaxyx stdscr)])
;;   (let* ([newPD  (mainWindow #:get-max-y-x)]
;;          [newWin (if (not (equal? (mainWindow #:get-max-y-x) pastDimensions))
;;                      (mainWindow #:rebuild)
;;                    mainWindow)]
;;          [char   (getch (newWin #:get-window))])
;;     (if (newWin #:is-in-mode)
;;         (cond
;;          [(equal? char KEY_LEFT)  (main (newWin   #:move-select -1) newPD)]
;;          [(equal? char KEY_RIGHT) (main (newWin   #:move-select  1) newPD)]
;;          [(equal? char #\=)       (main (newWin #:change-select  1) newPD)]
;;          [(equal? char #\-)       (main (newWin #:change-select -1) newPD)]
;;          [(not (equal? char #\q))                      (main newWin newPD)])
;;       (cond
;;        ;; [(equal? char #\a)       (main
;;        ;;                            (newWin #:add-new-line (car d) #f)
;;        ;;                            newPD
;;        ;;                            (cdr d))]
;;        ;; [(equal? char #\i)       (main
;;        ;;                            (newWin #:add-new-line (car d) 5)
;;        ;;                            newPD
;;        ;;                            (cdr d))]
;;        [(equal? char KEY_NPAGE)    (main (newWin #:move-cursor  10) newPD)]
;;        [(equal? char #\n)          (main (newWin #:move-cursor  3)  newPD)]
;;        [(equal? char KEY_DOWN)     (main (newWin #:move-cursor  1)  newPD)]
;;        [(equal? char KEY_PPAGE)    (main (newWin #:move-cursor -10) newPD)]
;;        [(equal? char #\p)          (main (newWin #:move-cursor -3)  newPD)]
;;        [(equal? char KEY_UP)       (main (newWin #:move-cursor -1)  newPD)]
;;        [(or
;;           (equal? char 13)
;;           (equal? char 10)
;;           (equal? char KEY_ENTER)
;;           (equal? char #\newline)) (newWin #:play)
;;                                    (main newWin newPD)]
;;        [(equal? char #\space)      (newWin #:toggle-play)
;;                                    (main newWin newPD)]
;;        [(equal? char #\v)          (newWin #:set-vol 5 #t)
;;                                    (main newWin newPD)]
;;        [(equal? char #\V)          (newWin #:set-vol 5 #f)
;;                                    (main newWin newPD)]
;;        [(equal? char #\esc)        (nodelay! (newWin #:get-window) #t)
;;                                    (let* ([w (newWin #:get-window)]
;;                                           [c             (getch w)])
;;                                      (nodelay! w #f)

;;                                      ;; If false, the user just pressed ESC
;;                                      (if c
;;                                          (begin
;;                                            (when (and
;;                                                    (equal? c         #\[)
;;                                                    (equal? (getch w) #\1)
;;                                                    (equal? (getch w) #\;))
;;                                              (let ([newC (getch w)])
;;                                                (cond
;;                                                 ;; Shift
;;                                                 [(equal? newC #\2)
;;                                                       (let ([nnC (getch w)])
;;                                                         (cond
;;                                                          [(equal? nnC #\C)
;;                                                                (newWin #:seek "+5")]
;;                                                          [(equal? nnC #\D)
;;                                                                (newWin #:seek "-5")]))]
;;                                                 ;; Alt
;;                                                 [(equal? newC #\3) ]
;;                                                 ;; Alt + Shift
;;                                                 [(equal? newC #\4) ]
;;                                                 ;; Ctrl
;;                                                 [(equal? newC #\5)
;;                                                       (let ([nnC (getch w)])
;;                                                         (cond
;;                                                          [(equal? nnC #\C)
;;                                                                (newWin #:seek "+60")]
;;                                                          [(equal? nnC #\D)
;;                                                                (newWin #:seek "-60")]))]
;;                                                 ;; Ctrl + Shift
;;                                                 [(equal? newC #\6)
;;                                                       (let ([nnC (getch w)])
;;                                                         (cond
;;                                                          [(equal? nnC #\C)
;;                                                                (newWin #:seek "+10")]
;;                                                          [(equal? nnC #\D)
;;                                                                (newWin #:seek "-10")]))]
;;                                                 ;; Ctrl + Alt
;;                                                 [(equal? newC #\7) ]
;;                                                 ;; Ctrl + Shift + Alt
;;                                                 [(equal? newC #\8) ])))

;;                                            (main newWin newPD))
;;                                        ;; Escape key, only
;;                                        (main newWin newPD)))]
;;        [(equal? char #\s)          (main (newWin #:enter-select) newPD)]
;;        [(not (equal? char #\q))    (main newWin newPD)]))))

(endwin)



;; (display (get-mpd-response
;;            (mpdDatabase::ls-info
;;              client
;;              (cdadar (get-mpd-response (mpdDatabase::list-all client))))))
;; (display db)
(newline)
(newline)
(mpd-connect    client)
;; (display (get-mpd-response (mpdDatabase::list-all client)))
(display (get-mpd-response (mpdPlaylistCurrent::playlist-info client)))
(mpd-disconnect client)
