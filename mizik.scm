#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./windows3.scm")

(setlocale LC_ALL "")

;; Initialize the main screen and settings
(define stdscr (initscr))
(use-default-colors)
(noecho!)
(cbreak!)
(start-color!)
(keypad! stdscr #t)
(refresh stdscr)

;; Initialize the MPD client
(define client (new-mpd-client))



;; (mpd-connect client)
;; (define db (map
;;              (lambda (y)
;;                (map
;;                  (lambda (x)
;;                    (if (number? (cdr x))
;;                        (cons (car x) (number->string (cdr x)))
;;                      x))
;;                  y))
;;              (get-mpd-response (mpdDatabase::ls-info client "Born to Run"))))
;; (map
;;   (lambda (song)
;;     (mpdPlaylistCurrent::add! client (assoc-ref song 'file)))
;;   (get-mpd-response (mpdDatabase::list-all client)))
;; (mpd-disconnect client)

(let main ([mainWindow     (begin
                             (mpd-connect client)

                             (mpdPlaylistCurrent::clear! client)
                             (mpdPlaylistCurrent::add!
                               client
                               (assoc-ref
                                 (car (get-mpd-response
                                        (mpdDatabase::list-all client)))
                                 'directory))

                             (let loop ([w (windows::build-columned-window
                                             stdscr   "Track"
                                             "Title"  "Genre"
                                             "Artist" "Album" "Time")]
                                        [s (get-mpd-response
                                             (mpdPlaylistCurrent::playlist-info
                                               client))])
                               (if (null? s)
                                   (begin
                                     (mpd-disconnect client)
                                     w)
                                 (loop
                                   (w #:add-new-line
                                        (map
                                          (lambda (x)
                                            (if (number? (cdr x))
                                                (cons
                                                  (car x)
                                                  (number->string (cdr x)))
                                              x))
                                          (car s))
                                        #f)
                                   (cdr s)))))]
           [pastDimensions (getmaxyx stdscr)])
    (let [(newPastDimensions (mainWindow #:get-max-y-x))]
      (mainWindow #:refresh)

      (let* ([newWin (if (not (equal?
                                (mainWindow #:get-max-y-x)
                                pastDimensions))
                         (mainWindow #:rebuild)
                       mainWindow)]
             [char   (getch (newWin #:get-window))])
        (cond
         ;; [(equal? char #\a)       (main
         ;;                            (newWin #:add-new-line (car d) #f)
         ;;                            newPastDimensions
         ;;                            (cdr d))]
         ;; [(equal? char #\i)       (main
         ;;                            (newWin #:add-new-line (car d) 5)
         ;;                            newPastDimensions
         ;;                            (cdr d))]
         [(equal? char KEY_NPAGE)    (main
                                       (newWin #:move-cursor  10)
                                       newPastDimensions)]
         [(equal? char #\n)          (main
                                       (newWin #:move-cursor  3)
                                       newPastDimensions)]
         [(equal? char KEY_DOWN)     (main
                                       (newWin #:move-cursor  1)
                                       newPastDimensions)]
         [(equal? char KEY_PPAGE)    (main
                                       (newWin #:move-cursor -10)
                                       newPastDimensions)]
         [(equal? char #\p)          (main
                                       (newWin #:move-cursor -3)
                                       newPastDimensions)]
         [(equal? char KEY_UP)       (main
                                       (newWin #:move-cursor -1)
                                       newPastDimensions)]
         [(or
            (equal? char #\b)
            (equal? char 13)
            (equal? char 10)
            (equal? char KEY_ENTER)
            (equal? char #\newline)) (newWin #:play client)
                                     (main newWin newPastDimensions)]
         [(equal? char #\space)      (mpd-connect    client)
                                     (if (string=?
                                           (assoc-ref
                                             (get-mpd-response
                                               (mpdStatus::status client))
                                             'state)
                                           "play")
                                         (mpdPlaybackControl::pause client #t)
                                       (mpdPlaybackControl::pause client #f))
                                     (mpd-disconnect client)
                                     (main newWin newPastDimensions)]
         [(not (equal? char #\q))    (main newWin newPastDimensions)]))))

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
