#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))

(define stdscr (initscr))

(let main ()
  (addstr  stdscr "Hello!")
  (refresh stdscr))

(endwin)
