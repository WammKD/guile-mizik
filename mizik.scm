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

(let main [(past_dimensions (getmaxyx stdscr))
           (num                             0)]
  ;; (move stdscr 1 0)
  ;; (refresh stdscr)
  ;; (header #:rebuild)
  ;; (getch   stdscr)
  ;; (header #:delete)
  (let [(shit (getmaxyx stdscr))]
    (refresh stdscr)
    (when (or
            (not (= (car  past_dimensions) (car  shit)))
            (not (= (cadr past_dimensions) (cadr shit))))
      (main-window #:rebuild))

    (when (< num 50000000)
      (main shit (1+ num)))))

(endwin)
