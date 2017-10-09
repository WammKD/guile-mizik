#!/usr/local/bin/guile
!#
(use-modules (mpd)
             (ncurses curses))
(include     "./windows.scm")

(setlocale LC_ALL "")

;; Initialize the main screen and settings
(define stdscr (initscr))
(use-default-colors)
(noecho!)
(cbreak!)
(start-color!)
(keypad! stdscr #t)
(refresh stdscr)

;; Initialize the MPD client
(define client (new-mpd-client))

(let main [(main_window     (windows::build-main-window stdscr  #f
                                                        "Track" "Title"
                                                        "Genre" "Artist"
                                                        "Album" "Time"))
           (past_dimensions                           (getmaxyx stdscr))]
  (let [(new_past_dimensions (main_window #:get-con-max-y-x))]
    (main_window #:refresh-contain)

    (let* [(new_win (if (not (equal?
                               (main_window #:get-con-max-y-x)
                               past_dimensions))
                        (main_window #:rebuild)
                      main_window))
           (char    (getch (new_win #:get-con-window)))]
      (cond
       [(equal? char #\a) (main (new_win
                                  #:add-line
                                    '((file . "Born to Run/Bruce Springsteen (Born to Run) - 01 - Thunder Road.mp3")
                                      (Last-Modified . "2017-06-04T19:17:11Z")
                                      (Time . "291")
                                      (Artist . "Bruce Springsteen")
                                      (AlbumArtist . "Bruce Springsteen")
                                      (Title . "Thunder Road")
                                      (Album . "Born to Run")
                                      (Track . "1/8")
                                      (Date . "1975")
                                      (Genre . "Rock")
                                      (Composer . "Bruce Springsteen")
                                      (Disc . "1")
                                      (MUSICBRAINZ_ARTISTID . "70248960-cb53-4ea4-943a-edb18f7d336f"))) new_past_dimensions)]
       [(not (equal? char #\q)) (main new_win new_past_dimensions)]))))

(endwin)


(mpd-connect client)

(display (get-mpd-response (mpdDatabase::ls-info client "Born to Run")))
;; (display (get-mpd-response
;;            (mpdDatabase::ls-info
;;              client
;;              (cdadar (get-mpd-response (mpdDatabase::list-all client))))))
(newline)

(mpd-disconnect client)
