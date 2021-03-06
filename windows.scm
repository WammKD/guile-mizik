#!/usr/local/bin/guile
!#
(use-modules (ncurses curses)
             (srfi srfi-1))

(define* (windows::build-header-bar containing_window new_window caption)
  (define (transform-line line)
    (let [(len      (getmaxx containing_window))
          (ELLIPSIS                         "…")]
      (if (= len 0)
          ""
        (if (or (= len 1) (= len 2))
            ELLIPSIS
          (if (> (string-length line) (- len 1))
              (string-append (substring line 0 (- len 2)) ELLIPSIS)
            line)))))

  (define window (if new_window
                     new_window
                   (let* [(len    (getmaxx containing_window))
                          (header (newwin
                                    1
                                    len
                                    (getbegy containing_window)
                                    (getbegx containing_window)))]
                     (addstr header (transform-line caption))
                     (chgat header -1 A_REVERSE 0 #:x 0 #:y 0)
                   
                     (refresh header)
                   
                     header)))

  (lambda (method . xs)
    (cond
     [(eq? method #:get-caption)                   caption]
     [(eq? method #:get-window)                     window]
     [(eq? method #:get-max-y-x)         (getmaxyx window)]
     [(eq? method #:get-max-y)           (getmaxy  window)]
     [(eq? method #:get-max-x)           (getmaxx  window)]
     [(eq? method #:transform)   (transform-line (car xs))]
     [(eq? method #:rebuild)     (resize
                                   window
                                   1
                                   (getmaxx containing_window))
                                 (mvwin
                                   window
                                   (getbegy containing_window)
                                   (getbegx containing_window))

                                 (erase window)

                                 (addstr window (transform-line caption))
                                 (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)

                                 (refresh window)

                                 (windows::build-header-bar
                                   containing_window
                                   window
                                   caption)]
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
                                    containing_window
                                    win
                                    (windows::build-header-bar win #f cc)
                                    '()
                                    0
                                    ratio)
                                  result)
                                (cdr current)
                                (+ offset rounded)))))))))

  (define (container containing_win win           header_bar
                     lines          highlight_pos percentage)
    (lambda (method . xs)
      (cond
       [(eq? method #:get-header-object)                 header_bar]
       [(eq? method #:get-caption)       (header_bar #:get-caption)]
       [(eq? method #:get-window)                               win]
       [(eq? method #:get-max-y-x)                   (getmaxyx win)]
       [(eq? method #:get-max-y)                     (getmaxy  win)]
       [(eq? method #:get-max-x)                     (getmaxx  win)]
       [(eq? method #:get-ratio)                         percentage]
       [(eq? method #:add-line)          (addstr
                                           win
                                           (header_bar #:transform (car xs))
                                           #:y (+ 1 (length lines))
                                           #:x 0)
                                         (refresh win)

                                         (container
                                           containing_win
                                           win
                                           (header_bar #:rebuild)
                                           (append lines (list (car xs)))
                                           highlight_pos
                                           percentage)]
       [(eq? method #:move-cursor)       (let* [(h_p- (1- highlight_pos))
                                                (h_p+ (1+ highlight_pos))
                                                (lens (length lines))
                                                (pos  (if (car xs)
                                                          (if (< h_p- 0)
                                                              0
                                                            h_p-)
                                                        (if (> h_p+ lens)
                                                            lens
                                                          h_p+)))]
                                           (when (not (= highlight_pos 0))
                                             (chgat win      -1
                                                    A_NORMAL 0
                                                    #:x      0
                                                    #:y      highlight_pos))

                                           (when (> pos 0)
                                             (chgat win -1 A_REVERSE 0
                                                    #:x 0  #:y       pos)
                                             (refresh win))

                                           (container
                                             containing_win
                                             win
                                             header_bar
                                             lines
                                             pos
                                             percentage))]
       [(eq? method #:rebuild)           (let* [(cw_len   (getmaxx
                                                            containing_win))
                                                (offset   (car  xs))
                                                (last_w?  (cadr xs))
                                                (nw_width (inexact->exact
                                                            (round
                                                              (*
                                                                percentage
                                                                cw_len))))]
                                           (resize
                                             win
                                             (getmaxy containing_win)
                                             (if last_w?
                                                 (- cw_len offset)
                                               nw_width))
                                           (mvwin
                                             win
                                             (getbegy containing_win)
                                             offset)
                                           (erase win)

                                           (let loop [(elems lines)
                                                      (index     1)]
                                             (when (not (null? elems))
                                               (addstr
                                                 win
                                                 (header_bar #:transform
                                                               (car elems))
                                                 #:y index
                                                 #:x 0)

                                               (loop (cdr elems) (1+ index))))

                                           (refresh win)

                                           (container
                                             containing_win
                                             win
                                             (header_bar #:rebuild)
                                             lines
                                             highlight_pos
                                             percentage))]
       [(eq? method #:erase)             (let [(pos (getyx containing_win))]
                                           (header_bar #:erase)

                                           (erase   win)
                                           (refresh win)

                                           (delwin  win)

                                           (move
                                             containing_win
                                             (car  pos)
                                             (cadr pos)))])))

  (lambda (method . xs)
    (cond
     [(eq? method #:get-windows)                          windows]
     [(eq? method #:get-con-window)             containing_window]
     [(eq? method #:get-con-max-y-x) (getmaxyx containing_window)]
     [(eq? method #:get-con-max-y)   (getmaxy  containing_window)]
     [(eq? method #:get-con-max-x)   (getmaxx  containing_window)]
     [(eq? method #:refresh-contain) (refresh  containing_window)]
     [(eq? method #:add-line)        (windows::build-main-window
                                       containing_window
                                       (map
                                         (lambda (window)
                                           (window
                                             #:add-line
                                             (let loop [(elements (car xs))]
                                               (if (null? elements)
                                                   ""
                                                 (if (eq?
                                                       (string->symbol
                                                         (window
                                                           #:get-caption))
                                                       (caar elements))
                                                     (cdar elements)
                                                   (loop (cdr elements)))))))
                                         windows))]
     [(eq? method #:move-cursor)     (windows::build-main-window
                                       containing_window
                                       (map
                                         (lambda (window)
                                           (window #:move-cursor (car xs)))
                                         windows))]
     [(eq? method #:rebuild)         (windows::build-main-window
                                       containing_window
                                       (let temp [(offset          0)
                                                  (remaining windows)
                                                  (final         '())]
                                         (if (null? (cdr remaining))
                                             (reverse
                                               (cons
                                                 ((car remaining) #:rebuild
                                                                    offset
                                                                    #t)
                                                 final))
                                           (let [(win ((car remaining)
                                                        #:rebuild
                                                          offset
                                                          #f))]
                                             (temp
                                               (+ offset (win #:get-max-x))
                                               (cdr remaining)
                                               (cons win final))))))]
     [(eq? method #:erase)           (for-each (lambda (x)
                                                 (x #:erase)) windows)])))
