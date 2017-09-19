#!/usr/local/bin/guile
!#
(use-modules (ncurses curses))

(define* (windows::build-header-bar containing_window caption)
  (define window (let [(header (newwin 1 (cadr (getmaxyx containing_window))
                                       0 0))]
                   (addstr header caption)
                   (chgat header -1 A_REVERSE 0 #:x 0 #:y 0)
                   
                   (refresh header)
                   
                   header))

  (lambda (method . xs)
    (cond
     [(eq? method #:get-window) window]
     [(eq? method #:get-maxyx)  (getmaxyx window)]
     [(eq? method #:rebuild)    (resize
                                  window
                                  1
                                  (cadr (getmaxyx containing_window)))
                                (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
                                (refresh window)]
     [(eq? method #:delete)     (let [(pos (getyx containing_window))]
                                  (for-each
                                    (lambda (x)
                                      (addch window (normal #\space)))
                                    (iota (cadr (getmaxyx window))))
                                  (refresh window)

                                  (delwin  window)

                                  (move
                                    containing_window
                                    (car pos)
                                    (cadr pos)))])))
