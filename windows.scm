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
  (define caption_ratios (let [(len   (getmaxx containing_window))
                               (total (fold
                                        (lambda (x y)
                                          (+ (* (string-length x) 1.0) y))
                                        0
                                        captions))]
                           (let temp [(result          '())
                                      (current    captions)
                                      (cumulative        0)]
                             (if (null? (cdr current))
                                 (reverse (cons (cons
                                                  (car current)
                                                  (- len cumulative)) result))
                               (let* [(cc      (car current))
                                      (rounded (inexact->exact
                                                 (round
                                                   (* (/ (string-length
                                                           cc) total) len))))]
                                 (temp
                                  (cons (cons cc rounded) result)
                                  (cdr current)
                                  (+ cumulative rounded)))))))

  (define (container win header-bar)
    (lambda  (method . xs)
      (cond
       [(eq? method #:get-header-object) header-bar]
       [(eq? method #:get-window)        win]
       [(eq? method #:get-maxyx)         (getmaxyx win)]
       [(eq? method #:erase)             (let [(pos (getyx containing_window))]
                                           (header-bar #:erase)

                                           (erase   win)
                                           (refresh win)

                                           (delwin  win)

                                           (move
                                             containing_window
                                             (car  pos)
                                             (cadr pos)))])))

  (define windows (let temp [(offset              0)
                             (result            '())
                             (c_rs   caption_ratios)]
                    (if (null? c_rs)
                        (reverse result)                              
                      (let* [(capt_ratio (car c_rs))
                             (len        (cdr capt_ratio))
                             (win        (newwin
                                           (getmaxy containing_window)
                                           len
                                           0
                                           offset))]
                        (refresh win)
                        ;; Can't build/refresh header before containing_window
                        (let [(header (windows::build-header-bar
                                        win
                                        (car capt_ratio)))]
                          (temp
                            (+ offset len)
                            (cons (container win header) result)
                            (cdr c_rs)))))))

  (lambda (method . xs)
    (cond
     [(eq? method #:get-captions-to-ratios-of-win) caption_ratios]
     [(eq? method #:get-windows)                          windows]
     [(eq? method #:rebuild)                       (apply
                                                     windows::build-main-window
                                                     (list
                                                       containing_window
                                                       captions))]
     [(eq? method #:erase)                         (for-each
                                                     (lambda (x)
                                                       (x #:erase))
                                                     windows)])))
