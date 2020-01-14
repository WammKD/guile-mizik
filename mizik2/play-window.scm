#!/usr/bin/guile
!#
(use-modules (ice-9 atomic) (ice-9 threads) (srfi srfi-9))
(include     "./play-window-status-info.scm")
(include     "./util.scm")

(define-record-type <mizik-play-window>
  (make-mizik-play-window window runningThread statusBox displayedSongBox)
  mizik-play-window?
  (window            the-curses-window  the-curses-window-set!)
  (runningThread        running-thread     running-thread-set!)
  (       statusBox         status-box         status-box-set!)
  (displayedSongBox displayed-song-box displayed-song-box-set!))

(define (generate-progress-bar win state elapsed totalTime isStopped)
  (define (parse-seconds seconds)
    (let* ([rounded        (inexact->exact (round seconds))]
           [mins    (number->string (quotient  rounded 60))]
           [secs    (number->string (remainder rounded 60))])
      (string-append mins ":" (if (= (string-length secs) 1)
                                  (string-append "0" secs)
                                secs))))

  (let* ([remaining                                     (- totalTime elapsed)]
         [eString                                   (parse-seconds   elapsed)]
         [rString                                   (parse-seconds remaining)]
         [tString                                   (parse-seconds totalTime)]
         [totalLength (- (cols)                  3 (string-length eString) 4
                         (string-length rString) 3 (string-length tString) 1)]
         [firstLength (if (zero? totalTime)
                          0
                        (inexact->exact
                          (floor (* (/ elapsed totalTime) totalLength))))]
         [finalString (string-append
                        state
                        eString
                        " ["
                        (string-concatenate/shared
                          (make-list
                            (pos-or-to-zero firstLength)
                            "="))
                        (if isStopped "-" ">")
                        (string-concatenate/shared
                          (make-list
                            (pos-or-to-zero (- totalLength firstLength 1))
                            "-"))
                        "] "
                        rString
                        " / "
                        tString
                        " ")])
    (addchstr win (inverse-on finalString) #:x 0 #:y 1)

    finalString))

(define (write-song-status win xstring len status)
  (let ([f (inverse-on (append
                         (let ([strLen (length xstring)])
                           (if (> strLen (- len 4))
                               (let ([sublist (list-head xstring (- len 5))]
                                     [ellip   (normal #\…)])
                                 (set-xchar-attr!
                                   ellip
                                   (xchar-attr (car (reverse sublist))))

                                 (append sublist (list ellip)))
                             (append
                               xstring
                               (make-list (- len strLen) (normal #\space)))))
                         (normal (string-append/shared
                                   " "
                                   (if (assoc-ref status 'repeat) "↺" " ")
                                   (if (assoc-ref status 'random) "⤭" " ")
                                   " "))))])
    (addchstr win f #:x 0 #:y 0)

    f))

(define (rebuild-play-window playWindow)
  (define window (the-curses-window playWindow))

  (clear  window)
  (mvwin  window (- (lines) PLAY_WINDOW_HEIGHT)      0)
  (resize window PLAY_WINDOW_HEIGHT             (cols))

  (call-with-new-thread (lambda () (render-play-window playWindow #f))))

(define (render-play-window playWin doRecurse)
  (define client           (new-mpd-client))
  (define winWidth         (cols))
  (define winDimens        (cons winWidth (lines)))
  (define window           (the-curses-window  playWin))
  (define        statusBox (status-box         playWin))
  (define displayedSongBox (displayed-song-box playWin))

  (mpd-connect client)
  (let ([status (get-mpd-response (mpdStatus::status client))])
    (mpd-disconnect client)

    (case (string->symbol (assoc-ref status 'state))
      [(stop)  (let ([prevInfo (atomic-box-ref statusBox)]
                     [dispSong (write-song-status window   (normal "")
                                                  winWidth status)])
                 (when (or
                         (not prevInfo)
                         (not (and
                                (prev-status-info-win-dimens=? prevInfo winDimens)
                                (prev-status-info-state=?      prevInfo " ▪ "))))
                   (let ([newStatus (generate-progress-bar window " ▪ " 0 0 #t)])
                     (atomic-box-set!
                       statusBox
                       (generate-play-window-prev-status-info
                         winDimens
                         newStatus
                         (make-mizik-time 0 0)))))

                 (atomic-box-set! displayedSongBox #f))]
      [(play)  (mpd-connect client)
               (let ([song (get-mpd-response
                             (mpdStatus::current-song client))])
                 (mpd-disconnect client)
                 (let* ([elapsed   (assoc-ref status 'elapsed)]
                        [time      (assoc-ref song   'Time)]
                        [newStatus (generate-progress-bar window " ▶ " elapsed time #f)]
                        [dispSong  (write-song-status
                                     window
                                     (append
                                       (normal " ")
                                       (bold   (assoc-ref song 'Title))
                                       (normal " from ")
                                       (bold   (assoc-ref song 'Album))
                                       (normal " by ")
                                       (bold   (assoc-ref song 'Artist)))
                                     winWidth
                                     status)])
                   (atomic-box-set!
                     statusBox
                     (generate-play-window-prev-status-info
                       winDimens
                       newStatus
                       (make-mizik-time elapsed time)))

                   (atomic-box-set! displayedSongBox song)))]
      [(pause) (let ([prevInfo (atomic-box-ref statusBox)])
                 (if (not (prev-status-info-win-dimens=? prevInfo winDimens))
                     (let* ([song      (atomic-box-ref displayedSongBox)]
                            [elapsed   (assoc-ref status 'elapsed)]
                            [time      (assoc-ref song   'Time)]
                            [newStatus (generate-progress-bar
                                         window  PAUSE_STATUS_ICON
                                         elapsed time              #f)]
                            [dispSong  (write-song-status
                                         window
                                         (append
                                           (normal " ")
                                           (bold   (assoc-ref song 'Title))
                                           (normal " from ")
                                           (bold   (assoc-ref song 'Album))
                                           (normal " by ")
                                           (bold   (assoc-ref song 'Artist)))
                                         winWidth
                                         status)])
                       (atomic-box-set!
                         statusBox
                         (generate-play-window-prev-status-info
                           winDimens
                           newStatus
                           (make-mizik-time elapsed time))))
                   (when (not (prev-status-info-state=? prevInfo PAUSE_STATUS_ICON))
                     (let ([newStatus (string-append
                                        PAUSE_STATUS_ICON
                                        (prev-status-info-status-string prevInfo))])
                       (addchstr window (inverse-on newStatus) #:x 0 #:y 1)

                       (atomic-box-set!
                         statusBox
                         (generate-play-window-prev-status-info
                           winDimens
                           newStatus
                           (prev-status-info-time prevInfo)))))))])

    (refresh window))

  (sleep 1)

  (when doRecurse
    (render-play-window playWin doRecurse)))

(define (generate-play-window columnWindowHeight)
  (let ([playWin (make-mizik-play-window
                   (newwin PLAY_WINDOW_HEIGHT (cols) columnWindowHeight 0)
                   #f
                   (make-atomic-box #f)
                   (make-atomic-box #f))])
    (running-thread-set! playWin (call-with-new-thread
                                   (lambda () (render-play-window playWin #t))))

    playWin))
