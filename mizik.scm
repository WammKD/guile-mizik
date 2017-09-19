#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./windows.scm")

(setlocale LC_ALL "")

(define stdscr (initscr))
(noecho!)
(cbreak!)
(start-color!)
(keypad! stdscr #t)
(refresh stdscr)

(define header (windows::build-header-bar stdscr "Blink Don't Blink"))

(let main [(past_dimensions (getmaxyx stdscr))]
  ;; (move stdscr 1 0)
  ;; (refresh stdscr)
  (header #:rebuild)
  (getch   stdscr)
  (header #:delete)
  (getch   stdscr)
  (getch   stdscr)
  (getch   stdscr))

(endwin)
