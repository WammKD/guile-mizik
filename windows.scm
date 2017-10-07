#!/usr/local/bin/guile
!#
(use-modules (ncurses curses)
             (srfi srfi-1))

(define* (windows::build-header-bar containing_window caption)
  (define (transform-caption)
    (let [(len      (getmaxx containing_window))
          (ELLIPSIS                         "…")]
      (if (< len 2)
          ""
        (if (= len 2)
            ELLIPSIS
          (if (> (string-length caption) (- len 1))
              (string-append (substring caption 0 (- len 2)) ELLIPSIS)
            caption)))))

  (define window (let* [(len    (getmaxx containing_window))
                        (header (newwin
                                  1
                                  len
                                  (getbegy containing_window)
                                  (getbegx containing_window)))]
                   (addstr header (transform-caption))
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
                                 (mvwin
                                   window
                                   (getbegy containing_window)
                                   (getbegx containing_window))
                                 (erase window)
                                 (addstr window (transform-caption))
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

(define (windows::build-main-window containing_window all_wins . captions)
  (define windows (if (or
                        (and      all_wins  (not (null? captions)))
                        (and (not all_wins)      (null? captions)))
                      (error (string-append
                               "In procedure windows::build-main-window: "
                               "Either windows must be passed or captions "
                               "to build windows from; both or neither can "
                               "be passed to this function."))
                    (if all_wins
                        all_wins
                      (let [(len    (getmaxx containing_window))
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
                                   (rounded (inexact->exact (round
                                                              (* ratio len))))
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
                                (+ offset rounded)))))))))

  (define (container win header_bar percentage)
    (lambda (method . xs)
      (cond
       [(eq? method #:get-header-object)     header_bar]
       [(eq? method #:get-window)                   win]
       [(eq? method #:get-maxyx)         (getmaxyx win)]
       [(eq? method #:get-ratio)             percentage]
       [(eq? method #:rebuild)           (let* [(cw_len   (getmaxx
                                                            containing_window))
                                                (offset   (car  xs))
                                                (last_w?  (cadr xs))
                                                (nw_width (inexact->exact
                                                            (round
                                                              (*
                                                                percentage
                                                                cw_len))))]
                                           (resize
                                             win
                                             (getmaxy containing_window)
                                             (if last_w?
                                                 (- cw_len offset)
                                               nw_width))
                                           (mvwin
                                             win
                                             (getbegy containing_window)
                                             offset)
                                           (refresh win)

                                           (header_bar #:rebuild)

                                           (+ nw_width offset))]
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
     [(eq? method #:rebuild)     (let temp [(offset          0)
                                            (remaining windows)]
                                   (if (null? (cdr remaining))
                                       ((car remaining) #:rebuild
                                                          offset
                                                          #t)
                                     (temp
                                       ((car remaining) #:rebuild
                                                          offset
                                                          #f)
                                       (cdr remaining))))]
     [(eq? method #:erase)       (for-each (lambda (x)
                                             (x #:erase)) windows)])))
