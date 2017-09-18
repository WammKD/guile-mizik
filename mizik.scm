#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))

(setlocale LC_ALL "")

(define stdscr (initscr))
(noecho!)
(cbreak!)
(start-color!)
(keypad! stdscr #t)
(refresh stdscr)

(define header (newwin 1 (cadr (getmaxyx stdscr)) 0 0))
(chgat header -1 A_REVERSE 0)
(addstr header "Blink Don't Blink")
(refresh header)

(let main [(past_dimensions (getmaxyx stdscr))]
  (move stdscr 1 0)
  (refresh stdscr)
  (getch   stdscr)
  (getch   stdscr)
  (getch   stdscr)
  (getch   stdscr))

(endwin)
