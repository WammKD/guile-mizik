#!/usr/local/bin/guile
!#
(use-modules (ncurses curses)
             (srfi srfi-1))

(define* (windows::build-header-bar containing_window caption)
  (define window (let* [(len    (getmaxx containing_window))
                        (header (newwin
                                  1
                                  len
                                  (getbegy containing_window)
                                  (getbegx containing_window)))
                        (capt   (if (> (string-length caption) (- len 1))
                                    (string-append
                                      (substring caption 0 (- len 2))
                                      "…")
                                  caption))]
                   (addstr header capt)
                   (chgat header -1 A_REVERSE 0 #:x 0 #:y 0)
                   
                   (refresh header)
                   
                   header))

  (lambda (method . xs)
    (cond
     [(eq? method #:get-caption) caption]
     [(eq? method #:get-window)  window]
     [(eq? method #:get-maxyx)   (getmaxyx window)]
     [(eq? method #:rebuild)     (resize
                                   window
                                   1
                                   (getmaxx containing_window))
                                 (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
                                 (refresh window)]
     [(eq? method #:erase)       (let [(pos (getyx containing_window))]
                                   (erase   window)
                                   (refresh window)

                                   (delwin  window)

                                   (move
                                     containing_window
                                     (car  pos)
                                     (cadr pos)))])))

(define (windows::build-main-window containing_window . captions)
  (define windows (let [(len    (getmaxx containing_window))
                        (height (getmaxy containing_window))
                        (total  (fold
                                  (lambda (x y)
                                    (+ (* (string-length x) 1.0) y))
                                  0
                                  captions))]
                    (let temp [(result       '())
                               (current captions)
                               (offset         0)]
                      (if (null? current)
                          (reverse result)
                        (let* [(cc      (car current))
                               (ratio   (/ (string-length cc) total))
                               (rounded (inexact->exact (round (* ratio len))))
                               (win     (newwin
                                          height
                                          (if (null? (cdr current))
                                              (- len offset)
                                            rounded)
                                          0
                                          offset))]
                          (refresh win)
                          ;; Can't build/refresh header before containing
                          (temp
                            (cons
                              (container
                                win
                                (windows::build-header-bar win cc)
                                ratio)
                              result)
                            (cdr current)
                            (+ offset rounded)))))))

  (define (container win header_bar percentage)
    (lambda (method . xs)
      (cond
       [(eq? method #:get-header-object)     header_bar]
       [(eq? method #:get-window)                   win]
       [(eq? method #:get-maxyx)         (getmaxyx win)]
       [(eq? method #:rebuild)           (resize
                                           window
                                           (getmaxy containing_window)
                                           )]
       [(eq? method #:erase)             (let [(pos (getyx containing_window))]
                                           (header_bar #:erase)

                                           (erase   win)
                                           (refresh win)

                                           (delwin  win)

                                           (move
                                             containing_window
                                             (car  pos)
                                             (cadr pos)))])))

  (lambda (method . xs)
    (cond
     [(eq? method #:get-windows) windows]
     [(eq? method #:rebuild)     (apply
                                   windows::build-main-window
                                   (list containing_window captions))]
     [(eq? method #:erase)       (for-each (lambda (x)
                                             (x #:erase)) windows)])))
