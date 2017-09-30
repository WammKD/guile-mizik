#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./windows.scm")

(setlocale LC_ALL "")

(define stdscr (initscr))
(refresh stdscr)
(noecho!)
(cbreak!)
(start-color!)
(keypad! stdscr #t)

(define left   (newwin
                 (car (getmaxyx stdscr))
                 (floor (/ (cadr (getmaxyx stdscr)) 3))
                 0
                 0))
(refresh left)
(define middle (newwin
                 (car (getmaxyx stdscr))
                 (floor (/ (cadr (getmaxyx stdscr)) 3))
                 0
                 (floor (/ (cadr (getmaxyx stdscr)) 3))))
(refresh middle)
(define right  (newwin
                 (car (getmaxyx stdscr))
                 (floor (/ (cadr (getmaxyx stdscr)) 3))
                 0
                 (* (floor (/ (cadr (getmaxyx stdscr)) 3)) 2)))
(refresh right)

(define   left-header (windows::build-header-bar left   "Blink Don't Blink"))
(define middle-header (windows::build-header-bar middle "Fuck"))
(define  right-header (windows::build-header-bar right  "Damn"))

(let main [(past_dimensions (getmaxyx stdscr))]
  ;; (move stdscr 1 0)
  ;; (refresh stdscr)
  ;; (header #:rebuild)
  ;; (getch   stdscr)
  ;; (header #:delete)
  (getch   stdscr)
  (getch   stdscr)
  (getch   stdscr))

(endwin)
