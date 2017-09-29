#!/usr/local/bin/guile
!#
(use-modules (ncurses curses))

(define* (windows::build-header-bar containing_window caption)
  (define window (let* [(len    (cadr (getmaxyx containing_window)))
                        (header (newwin
                                  1
                                  len
                                  (getbegy containing_window)
                                  (getbegx containing_window)))
                        (capt   (if (> (string-length caption) len)
                                    (string-append
                                      (substring caption 0 (- len 1))
                                      "…")
                                  caption))]
                   (addstr header capt)
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
