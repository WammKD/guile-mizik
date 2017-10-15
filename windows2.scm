#!/usr/local/bin/guile
!#
(use-modules (ncurses curses)
             (srfi    srfi-1))

(define* (: received_atom s_i #:optional [e_i (length received_atom)])
  (let* ([lora        (length received_atom)]
         [start_index (if (negative? s_i)
                          (if (negative? (+ lora s_i)) 0 (+ lora s_i))
                        s_i)]
         [end_index   (if (negative? e_i)
                          (if (> (+ lora e_i) lora) lora (+ lora e_i))
                        (if (> e_i lora) lora e_i))])
    (if (> start_index end_index)
        '()
      (let* ([list_head        (list-head received_atom end_index)]
             [len_of_list_head                  (length list_head)])
        (list-tail list_head (if (> start_index len_of_list_head)
                                 len_of_list_head
                               start_index))))))

(define* (windows::build-columned-window window        highlight_pos
                                         all_columns . captions)
  (define* (column lines    offset   percentage window_length
                   view_beg view_end #:optional [formatted #f])
    (define column_length (if (exact? percentage)
                              percentage
                            (inexact->exact
                              (round (* percentage window_length)))))

    (define (calc-new-line line)
      (let ([ELLIPSIS "…"])
        (if (<= column_length 0)
            ""
          (if (= column_length 1)
              ELLIPSIS
            (if (= column_length 2)
                (string-append ELLIPSIS " ")
              (let ([line_length (string-length line)])
                (if (> line_length (- column_length 1))
                    (string-append
                      (substring line 0 (- column_length 2))
                      ELLIPSIS
                      " ")
                  (let loop ([count (- column_length line_length)]
                             [final                          line])
                    (if (= count 0)
                        final
                      (loop (1- count) (string-append final " ")))))))))))

    (let ([new_lines (if formatted
                         formatted
                       (let loop ([lst    (cons
                                            (car lines)
                                            (: lines view_beg view_end))]
                                  [result                            '()]
                                  [index                               0])
                         (if (null? lst)
                             (reverse result)
                           (let ([format (calc-new-line (car lst))])
                             (addstr window format #:y index #:x offset)

                             (loop (cdr lst) (cons
                                               format
                                               result) (1+ index))))))]
          [lines_len (length lines)])
      (lambda (method . xs)
        (cond
         [(eq? method #:get-view-beg-pos)            view_beg]
         [(eq? method #:get-view-end-pos)            view_end]
         [(eq? method #:get-length)             column_length]
         [(eq? method #:get-header)           (car     lines)]
         [(eq? method #:get-formatted-header) (car new_lines)]
         [(eq? method #:get-lines)                      lines]
         [(eq? method #:calc-new-line)          calc-new-line]
         [(eq? method #:add-new-line)         (let* ([c_xs     (car xs)]
                                                     [new_line (calc-new-line
                                                                 c_xs)])
                                                (addstr
                                                  window new_line
                                                  #:y    (length new_lines)
                                                  #:x    offset)

                                                (column
                                                  (append lines (list c_xs))
                                                  offset
                                                  percentage
                                                  window_length
                                                  view_beg
                                                  (if (>
                                                        (1+ lines_len)
                                                        (getmaxy window))
                                                      view_end
                                                    (1+ view_end))
                                                  (append
                                                    new_lines
                                                    (list new_line))))]
         [(eq? method #:shift-page)           (column
                                                lines
                                                offset
                                                percentage
                                                window_length
                                                ((if (car xs)
                                                     1-
                                                   1+) view_beg)
                                                ((if (car xs)
                                                     1-
                                                   1+) view_end))]
         [(eq? method #:rebuild)              (let* ([win_h (getmaxy window)]
                                                     [half  (inexact->exact
                                                              (floor
                                                                (/
                                                                  (1- win_h)
                                                                  2.0)))]
                                                     [new_p (+
                                                              (cadddr xs)
                                                              (1- view_beg))]
                                                     [new_b (if (< (-
                                                                     new_p
                                                                     half) 1)
                                                                1
                                                              (- new_p half))]
                                                     [pos_e (+
                                                              new_b
                                                              (1- win_h))]
                                                     [pos_b (if (>
                                                                  pos_e
                                                                  lines_len)
                                                                (-
                                                                  lines_len
                                                                  (1- win_h))
                                                              new_b)])
                                                (column
                                                  lines
                                                  (car xs)
                                                  (if (cadr xs)
                                                      (cadr xs)
                                                    percentage)
                                                  (caddr xs)
                                                  (if (>= win_h lines_len)
                                                      1
                                                    pos_b)
                                                  (if (>= win_h lines_len)
                                                      lines_len
                                                    (if (>
                                                          pos_e
                                                          lines_len)
                                                        lines_len
                                                      pos_e))))]))))

  (define* (build-columns rebuild? #:optional)
    (let ([window_width  (getmaxx window)]
          [caption_total (if rebuild?
                             -1
                           (fold
                             (lambda (elem ret)
                               (+ (* (string-length elem) 1.0) ret))
                             0
                             captions))])
      (let loop ([result                             '()]
                 [current (if rebuild? columns captions)]
                 [off_set                              0])
        (if (null? (cdr current))
            (reverse (cons (if rebuild?
                               ((car current) #:rebuild
                                                off_set
                                                (- window_width off_set)
                                                window_width
                                                highlight_pos)
                             (column
                               (list (car current))
                               off_set
                               (- window_width off_set)
                               window_width
                               1
                               1)) result))
          (let* ([cc  (car current)]
                 [col (if rebuild?
                          (cc #:rebuild off_set #f window_width highlight_pos)
                        (column
                          (list cc)
                          off_set
                          (/ (string-length cc) caption_total)
                          window_width
                          1
                          1))])
            (loop
              (cons col result)
              (cdr current)
              (+ (col #:get-length) off_set)))))))

  (define columns (if (or
                        (and      all_columns  (not (null? captions)))
                        (and (not all_columns)      (null? captions)))
                      (error (string-append
                               "In procedure windows::build-columned-window: "
                               "Either columns must be passed or captions "
                               "to build columns from; both or neither can "
                               "not be passed to this function."))
                    (if all_columns
                        all_columns
                      (build-columns #f))))

  (chgat window -1 A_REVERSE 0 #:x 0 #:y 0)
  (chgat window -1 A_REVERSE 0 #:x 0 #:y highlight_pos)

  (lambda (method . xs)
    (cond
     [(eq? method #:get-window)              window]
     [(eq? method #:get-max-y-x)  (getmaxyx window)]
     [(eq? method #:get-max-y)    (getmaxy  window)]
     [(eq? method #:get-max-x)    (getmaxx  window)]
     [(eq? method #:refresh)      (refresh  window)]
     [(eq? method #:move-cursor)  (let* ([first_col           (car columns)]
                                         [new_pos     (if (car xs)
                                                          (1- highlight_pos)
                                                        (1+ highlight_pos))]
                                         [  win_len        (getmaxy window)]
                                         [lines_len (length (first_col
                                                              #:get-lines))])
                                    (if (and
                                          (> new_pos 0)
                                          (< new_pos (if (> lines_len win_len)
                                                         win_len
                                                       lines_len)))
                                        (begin
                                          (when (not (and
                                                       (= new_pos 1)
                                                       (= highlight_pos 0)))
                                            (chgat window   -1
                                                   A_NORMAL 0
                                                   #:x      0
                                                   #:y      highlight_pos))

                                          (windows::build-columned-window
                                            window
                                            new_pos
                                            columns))
                                      (let ([beg (first_col
                                                   #:get-view-beg-pos)]
                                            [end (first_col
                                                   #:get-view-end-pos)])
                                        (if (or
                                              (or
                                                (and
                                                  (or
                                                    (=
                                                      (1- win_len)
                                                      highlight_pos)
                                                    (=
                                                      (1- lines_len)
                                                      highlight_pos))
                                                  (= end lines_len))
                                                (and
                                                  (= 1 beg)
                                                  (= 1 highlight_pos)))
                                              (= new_pos -1))
                                            (windows::build-columned-window
                                              window
                                              highlight_pos
                                              columns)
                                          (windows::build-columned-window
                                            window
                                            highlight_pos
                                            (map
                                              (lambda (col)
                                                (col #:shift-page (car xs)))
                                              columns))))))]
     [(eq? method #:add-new-line) (windows::build-columned-window
                                    window
                                    highlight_pos
                                    (map
                                      (lambda (col)
                                        (col
                                          #:add-new-line
                                          (let loop [(elements (car xs))]
                                            (if (null? elements)
                                                ""
                                              (if (eq?
                                                    (string->symbol
                                                      (col #:get-header))
                                                    (caar elements))
                                                  (cdar elements)
                                                (loop (cdr elements)))))))
                                      columns))]
     [(eq? method #:rebuild)      (let* ([new_cols   (build-columns #t)]
                                         [new_beg    ((car new_cols)
                                                       #:get-view-beg-pos)]
                                         [orig_beg   ((car columns)
                                                       #:get-view-beg-pos)]
                                         [last_pos   (+
                                                       orig_beg
                                                       highlight_pos)]
                                         [win_height (getmaxy window)]
                                         [half       (inexact->exact
                                                       (floor
                                                         (/
                                                           (1- win_height)
                                                           2.0)))])
                                    (windows::build-columned-window
                                      window
                                      (if (< highlight_pos win_height)
                                          (-
                                            (+ orig_beg highlight_pos)
                                            new_beg)
                                        (if (< (-
                                                 (length
                                                   ((car new_cols)
                                                     #:get-lines))
                                                 last_pos) half)
                                            (-
                                              (1- win_height)
                                              (- (length
                                                   ((car new_cols)
                                                     #:get-lines)) last_pos))
                                          (1+ half)))
                                      new_cols))])))
