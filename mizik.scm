#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./windows2.scm")

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



(mpd-connect client)
(define db (map
             (lambda (y)
               (map
                 (lambda (x)
                   (if (number? (cdr x))
                       (cons (car x) (number->string (cdr x)))
                     x))
                 y))
             (get-mpd-response (mpdDatabase::ls-info client "Born to Run"))))
(mpd-disconnect client)



(let main ([main_window     (windows::build-columned-window stdscr  0     #f
                                                            "Track" "Title"
                                                            "Genre" "Artist"
                                                            "Album" "Time")]
           [past_dimensions                               (getmaxyx stdscr)]
           [d                                                            db])
  (let [(new_past_dimensions (main_window #:get-max-y-x))]
    (main_window #:refresh)

    (let* [(new_win (if (not (equal?
                               (main_window #:get-max-y-x)
                               past_dimensions))
                        (main_window #:rebuild)
                      main_window))
           (char    (getch (new_win #:get-window)))]
      (cond
       [(equal? char #\a) (main (new_win
                                  #:add-new-line
                                    (car d)) new_past_dimensions (cdr d))]
       [(equal? char #\n) (main
                            (new_win #:move-cursor #f)
                            new_past_dimensions
                            d)]
       [(equal? char #\p) (main
                            (new_win #:move-cursor #t)
                            new_past_dimensions
                            d)]
       [(not (equal? char #\q)) (main new_win new_past_dimensions d)]))))

(endwin)



;; (display (get-mpd-response
;;            (mpdDatabase::ls-info
;;              client
;;              (cdadar (get-mpd-response (mpdDatabase::list-all client))))))
(display db)
(newline)

