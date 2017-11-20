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

                             (mpdPlaybackOption::consume! client #f)

                             (mpdPlaylistCurrent::clear! client)
                             (mpdPlaylistCurrent::add!
                               client
                               (assoc-ref
                                 (car (get-mpd-response
                                        (mpdDatabase::list-all client)))
                                 'directory))

                             (let loop ([w (windows::build-columned-window
                                             stdscr
                                             client
                                             (cons
                                               "Track"
                                               (lambda (track)
                                                 (let ([index (string-index
                                                                track
                                                                #\/)])
                                                   (if index
                                                       (substring
                                                         track
                                                         0
                                                         index)
                                                     track))))
                                             (cons "Title"  (lambda (title)
                                                              title))
                                             (cons "Genre"  (lambda (genre)
                                                              genre))
                                             (cons "Artist" (lambda (artist)
                                                              artist))
                                             (cons "Album"  (lambda (album)
                                                              album))
                                             (cons
                                               "Time"
                                               (lambda (time)
                                                 (let* ([r (inexact->exact
                                                             (round
                                                               (string->number
                                                                 time)))]
                                                        [m (number->string
                                                             (quotient r 60))]
                                                        [s (number->string
                                                             (remainder
                                                               r
                                                               60))])
                                                   (string-append
                                                     m
                                                     ":"
                                                     (if (= (string-length
                                                              s) 1)
                                                         (string-append "0" s)
                                                       s))))))]
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
            (equal? char #\newline)) (newWin #:play        client)
                                     (main newWin newPastDimensions)]
         [(equal? char #\space)      (newWin #:toggle-play client)
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
