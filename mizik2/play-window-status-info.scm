#!/usr/bin/guile
!#
(use-modules (srfi srfi-9))

(define-record-type <mizik-play-window-status-info-time>
  (make-mizik-time elapsedTime totalTime)
  mizik-time?
  (elapsedTime elapsed-time elapsed-time-set!)
  (  totalTime   total-time   total-time-set!))

(define (generate-play-window-prev-status-info windowDimensions statusString time)
  (list
    windowDimensions
    (substring statusString 0 3)
    (substring statusString 3)
    (if (mizik-time? time)
        time
      (error (string-append
               "In procedure generate-play-window-status-info: "
               "time given is not of type "
               "<mizik-play-window-status-info-time>")))))

(define (prev-status-info-state=? prevStatusInfo state)
  (string=? (cadr prevStatusInfo) state))

(define (prev-status-info-win-dimens=? prevStatusInfo winDimensions)
  (equal? winDimensions (prev-status-info-window-dimensions prevStatusInfo)))

(define (prev-status-info-window-dimensions prevStatusInfo)
  (car prevStatusInfo))

(define (prev-status-info-status-string prevStatusInfo)
  (caddr prevStatusInfo))

(define (prev-status-info-time prevStatusInfo)
  (cadddr prevStatusInfo))
