#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./windows.scm")

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

;; (define main-window (windows::build-main-window stdscr "Blink Don't Blink"
;;                                                 "Fuck" "Damn"))

(define main-window (windows::build-main-window
                      stdscr
                      #f
                      "Track"
                      "Title"
                      "Genre"
                      "Artist"
                      "Album"
                      "Time"))

(let main [(main_window (windows::build-main-window stdscr  #f
                                                    "Track" "Title"
                                                    "Genre" "Artist"
                                                    "Album" "Time"))
           (past_dimensions                       (getmaxyx stdscr))
           (num                                                   0)]
  ;; (move stdscr 1 0)
  ;; (refresh stdscr)
  ;; (header #:rebuild)
  ;; (getch   stdscr)
  ;; (header #:delete)
  (let [(new_past_dimensions (main_window #:get-con-max-y-x))]
    (main_window #:refresh-contain)

    (let [(new_win (if (not (equal?
                              (main_window #:get-con-max-y-x)
                              past_dimensions))
                       (main_window #:rebuild)
                     main_window))]

      (when (< num 50000000)
        (main new_win new_past_dimensions (1+ num))))))

(endwin)
