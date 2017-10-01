#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./windows.scm")

(setlocale LC_ALL "")

;; Initialize the main screen and settings
(define stdscr (initscr))
(refresh stdscr)
(noecho!)
(cbreak!)
(start-color!)
(keypad! stdscr #t)

;; Initialize the MPD client
(define client (new-mpd-client))

(define main (windows::build-main-window stdscr "Blink Don't Blink"
                                         "Fuck" "Damn"))

(let main [(past_dimensions (getmaxyx stdscr))]
  ;; (move stdscr 1 0)
  ;; (refresh stdscr)
  ;; (header #:rebuild)
  ;; (getch   stdscr)
  ;; (header #:delete)
  (getch   stdscr)
  ;; (middle-header #:erase)
  (getch   stdscr)
  (getch   stdscr))

(endwin)
