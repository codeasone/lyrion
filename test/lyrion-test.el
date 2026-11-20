;;; lyrion-test.el --- Tests for lyrion -*- lexical-binding: t -*-

;; SPDX-License-Identifier: CC0-1.0

;;; Commentary:

;; Run with `make test'. The connection tests start a fake command-line
;; server and a fake web server on loopback ports, so no Lyrion is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lyrion)

;; The search menu's saved switches are the user's, not the tests'.
(setq transient-values nil)

;;; Helpers

(defun lyrion-test--wait (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, or TIMEOUT seconds pass.
Return PREDICATE's last value."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(defvar lyrion-test--received nil
  "Commands the fake server received, each a list of words, oldest first.")

(defvar lyrion-test--setup nil
  "Crossfade setup commands the fake server received, oldest first.
They are kept apart so the other commands can be checked on their own.")

(defun lyrion-test--setup-p (words)
  "Return non-nil when WORDS set up the following of the crossfade setting."
  (or (equal words '("subscribe" "prefset"))
      (and (equal (cadr words) "playerpref") (equal (nth 3 words) "?"))))

(defvar lyrion-test--clients nil
  "Connections the fake server accepted, newest first.")

(defmacro lyrion-test--with-server (handler &rest body)
  "Run BODY with a fake command-line server as `lyrion-host'.
HANDLER is called with each command's words and returns the reply to send,
or nil to send nothing."
  (declare (indent 1))
  `(let* ((lyrion-test--received nil)
          (lyrion-test--setup nil)
          (lyrion-test--clients nil)
          (handler ,handler)
          (server
           (make-network-process
            :name "lyrion-fake" :server t :host "127.0.0.1" :service t
            :family 'ipv4 :coding 'binary :noquery t
            :log (lambda (_server client _message)
                   (push client lyrion-test--clients))
            :filter
            (lambda (client chunk)
              (let ((lines (split-string
                            (concat (or (process-get client 'pending) "") chunk)
                            "\n")))
                (process-put client 'pending (car (last lines)))
                (dolist (line (butlast lines))
                  (let ((words (lyrion--decode line)))
                    (if (lyrion-test--setup-p words)
                        (setq lyrion-test--setup
                              (append lyrion-test--setup (list words)))
                      (setq lyrion-test--received
                            (append lyrion-test--received (list words))))
                    (when-let* ((reply (funcall handler words)))
                      (process-send-string client reply))))))))
          (lyrion-host "127.0.0.1")
          (lyrion-cli-port (process-contact server :service)))
     (unwind-protect
         (progn ,@body)
       (dolist (client lyrion-test--clients)
         (when (process-live-p client) (delete-process client)))
       (delete-process server))))

(defvar lyrion-test--requests nil
  "Paths the fake web server was asked for, oldest first.")

(defmacro lyrion-test--with-http (response &rest body)
  "Run BODY with a fake web server as `lyrion-host' answering RESPONSE.
RESPONSE is the whole HTTP response, a function of the requested path
returning it, or nil for a server that never answers. The server is also
`lyrion-lyrics-url', over plain HTTP."
  (declare (indent 1))
  `(let* ((response ,response)
          (lyrion-test--requests nil)
          (server
           (make-network-process
            :name "lyrion-fake-http" :server t :host "127.0.0.1" :service t
            :family 'ipv4 :coding 'binary :noquery t
            :filter
            (lambda (client chunk)
              (let* ((path (and (string-match "\\`GET \\([^ ]+\\)" chunk)
                                (match-string 1 chunk)))
                     (answer (if (functionp response)
                                 (funcall response path)
                               response)))
                (setq lyrion-test--requests
                      (append lyrion-test--requests (list path)))
                (when answer
                  (process-send-string client answer)
                  (process-send-eof client))))))
          (port (process-contact server :service))
          (lyrion-host "127.0.0.1")
          (lyrion-http-port port)
          (lyrion-lyrics-url (format "http://127.0.0.1:%d/api" port)))
     (unwind-protect
         (progn ,@body)
       (delete-process server))))

(defun lyrion-test--players-line ()
  "Return a `players' reply: a connected WiiM, and a stale local player.
The local player has lost its connection but still claims to be playing."
  (lyrion--encode
   '("players" "0" "99" "count:2"
     "playerindex:0" "playerid:02:00:00:00:00:01" "name:WiiM Ultra"
     "isplaying:0" "connected:1"
     "playerindex:1" "playerid:02:37:a6:bc:f5:7a" "name:Emacs on example"
     "isplaying:1" "connected:0")))

(defun lyrion-test--status-line (player &rest fields)
  "Return a status line for PLAYER with FIELDS, strings of the form KEY:VALUE."
  (lyrion--encode (append (list player "status" "-" "2" "tags:alcdyKtiq"
                                "subscribe:0" "player_name:WiiM Ultra"
                                "player_connected:1")
                          fields)))

(defconst lyrion-test--library
  '(("artists" "id:9" "artist:Arvo Pärt" "id:12" "artist:Al Green")
    ("genres" "id:3" "genre:Classical" "id:4" "genre:Soul")
    ("albums" "id:31" "album:Alina" "artist:Arvo Pärt" "year:1999"
     "id:32" "album:Let's Stay Together" "artist:Al Green" "year:1972")
    ("titles" "id:501" "title:Für Alina" "artist:Arvo Pärt" "album:Alina"
     "id:502" "title:Spiegel im Spiegel" "artist:Arvo Pärt" "album:Alina"
     "id:503" "title:Spiegel im Spiegel" "artist:Arvo Pärt" "album:Alina"
     "id:601" "title:Let's Stay Together" "artist:Al Green"
     "album:Let's Stay Together")
    ("works" "work_id:7" "composer:Arvo Pärt" "work:Für Alina"
     "composer_id:9" "album_id:31"
     "work_id:8" "composer:Arvo Pärt" "work:Spiegel im Spiegel"
     "composer_id:9" "album_id:31,32")
    ("playlists" "id:71" "playlist:Sunday Morning"))
  "A small library, as the fields each listing command answers with.
Spiegel im Spiegel is on Alina twice, as it is on the record. As a work it
is on two releases, so that it has two recordings.")

(defun lyrion-test--listing (words)
  "Answer WORDS, a listing command, from `lyrion-test--library', or nil."
  (when-let* ((fields (cdr (assoc (car words) lyrion-test--library))))
    (lyrion--encode (append words
                            (list (format "count:%d"
                                          (cl-count-if (lambda (field)
                                                         (string-prefix-p "id:" field))
                                                       fields)))
                            fields))))

(defvar lyrion-test--transition-type "0"
  "The transition type the fake server's players have.")

(defun lyrion-test--answer (words)
  "Answer WORDS as a server with a WiiM playing Für Alina would."
  (cond ((lyrion-test--listing words))
        ((equal (car words) "players") (lyrion-test--players-line))
        ((equal (cdr words) '("playerpref" "transitionType" "?"))
         (lyrion--encode (append (butlast words)
                                 (list lyrion-test--transition-type))))
        ((equal (cdr words) '("playerpref" "transitionSmart" "?"))
         (lyrion--encode (append (butlast words) '("1"))))
        ((equal (cdr words) '("playerpref" "transitionDuration" "?"))
         (lyrion--encode (append (butlast words) '("10"))))
        ((equal (cdr words) '("status" "0" "100000" "tags:al"))
         (lyrion--encode
          (append words
                  '("player_name:WiiM Ultra" "playlist_cur_index:1"
                    "playlist_tracks:3"
                    "playlist index:0" "id:501" "title:Für Alina"
                    "artist:Arvo Pärt" "album:Alina"
                    "playlist index:1" "id:502" "title:Spiegel im Spiegel"
                    "artist:Arvo Pärt" "album:Alina"
                    "playlist index:2" "id:601" "title:Let's Stay Together"
                    "artist:Al Green" "album:Let's Stay Together"))))
        ((equal (cadr words) "status")
         (lyrion-test--status-line (car words) "mode:play" "time:60"
                                   "duration:600" "playlist index:0"
                                   "title:Für Alina" "artist:Arvo Pärt"
                                   "album:Alina" "year:1999"))))

(defmacro lyrion-test--with-player (&rest body)
  "Run BODY in a fresh player buffer connected to the fake server."
  (declare (indent 0))
  `(let ((buffer (get-buffer-create lyrion-buffer-name)))
     (unwind-protect
         (with-current-buffer buffer
           (lyrion-mode)
           (lyrion--connect buffer)
           ,@body)
       (kill-buffer buffer))))

(defun lyrion-test--shows (text)
  "Return non-nil when the player buffer shows TEXT."
  (with-current-buffer lyrion-buffer-name
    (string-search text (buffer-string))))

;;; The protocol

(ert-deftest lyrion-test/decode-reads-utf-8-escapes ()
  (should (equal (lyrion--decode "title:F%C3%BCr%20Alina artist:Arvo%20P%C3%A4rt")
                 '("title:Für Alina" "artist:Arvo Pärt"))))

(ert-deftest lyrion-test/encode-round-trips ()
  (let ((words '("02:00:00:00:00:01" "status" "-" "2" "tags:alcdyKtiq"
                 "title:Für Alina")))
    (should (string-suffix-p "\n" (lyrion--encode words)))
    (should-not (string-search " " (car (split-string (lyrion--encode words)))))
    (should (equal (lyrion--decode (string-trim-right (lyrion--encode words)))
                   words))))

(ert-deftest lyrion-test/fields-split-at-the-first-colon ()
  (should (equal (lyrion--fields '("players" "playerid:02:00" "name:WiiM"))
                 '(("playerid" . "02:00") ("name" . "WiiM")))))

(ert-deftest lyrion-test/parse-players ()
  (let ((players (lyrion--parse-players
                  (lyrion--decode (string-trim-right (lyrion-test--players-line))))))
    (should (= 2 (length players)))
    (should (equal (lyrion--get (car players) "playerid") "02:00:00:00:00:01"))
    (should (equal (lyrion--get (cadr players) "name") "Emacs on example"))
    (should-not (lyrion--get (car players) "count"))))

(ert-deftest lyrion-test/parse-status-skips-the-player-id ()
  (let ((status (lyrion--parse-status
                 (lyrion--decode
                  (string-trim-right
                   (lyrion-test--status-line "02:00:00:00:00:01" "mode:pause"
                                             "playlist index:3"
                                             "title:Für Alina"))))))
    (should-not (assoc "54" status))
    (should (equal (lyrion--get status "mode") "pause"))
    (should (equal (lyrion--get status "playlist index") "3"))
    (should (equal (lyrion--get status "title") "Für Alina"))))

;;; Choosing the player

(ert-deftest lyrion-test/choose-player-passes-over-a-stale-player ()
  "A player that lost its connection but claims to play is not chosen."
  (let ((players (lyrion--parse-players
                  (lyrion--decode (string-trim-right (lyrion-test--players-line))))))
    (should (equal (lyrion--get (lyrion--choose-player players nil) "name")
                   "WiiM Ultra"))))

(ert-deftest lyrion-test/choose-player-prefers-the-one-playing ()
  (let ((players '((("name" . "Kitchen") ("isplaying" . "0") ("connected" . "1"))
                   (("name" . "Study") ("isplaying" . "1") ("connected" . "1")))))
    (should (equal (lyrion--get (lyrion--choose-player players nil) "name")
                   "Study"))))

(ert-deftest lyrion-test/choose-player-by-name-or-id ()
  (let ((players (lyrion--parse-players
                  (lyrion--decode (string-trim-right (lyrion-test--players-line))))))
    (should (equal (lyrion--get (lyrion--choose-player players "emacs on EXAMPLE")
                                "playerid")
                   "02:37:a6:bc:f5:7a"))
    (should (equal (lyrion--get (lyrion--choose-player players "02:00:00:00:00:01")
                                "name")
                   "WiiM Ultra"))
    (should-not (lyrion--choose-player players "Garage"))))

(ert-deftest lyrion-test/choose-player-falls-back-to-the-first ()
  (let ((players '((("name" . "A") ("connected" . "0"))
                   (("name" . "B") ("connected" . "0")))))
    (should (equal (lyrion--get (lyrion--choose-player players nil) "name") "A"))
    (should-not (lyrion--choose-player nil nil))))

;;; Time and progress

(ert-deftest lyrion-test/elapsed-counts-on-while-playing ()
  (let ((status '(("mode" . "play") ("time" . "60") ("duration" . "600"))))
    (should (= (lyrion--elapsed status 100.0 112.5) 72.5))
    (should (= (lyrion--elapsed status 100.0 2000.0) 600))))

(ert-deftest lyrion-test/elapsed-stands-still-when-paused ()
  (let ((status '(("mode" . "pause") ("time" . "60") ("duration" . "600"))))
    (should (= (lyrion--elapsed status 100.0 500.0) 60))))

(ert-deftest lyrion-test/elapsed-without-a-length ()
  "A stream has no length, so the time is not capped."
  (let ((status '(("mode" . "play") ("time" . "5000"))))
    (should (= (lyrion--elapsed status 0.0 10.0) 5010.0))))

(ert-deftest lyrion-test/format-time ()
  (should (equal (lyrion--format-time 0) "0:00"))
  (should (equal (lyrion--format-time 5.9) "0:05"))
  (should (equal (lyrion--format-time 626) "10:26"))
  (should (equal (lyrion--format-time 3723) "1:02:03"))
  (should (equal (lyrion--format-time -3) "0:00")))

(ert-deftest lyrion-test/progress-bar-fills-in-proportion ()
  (let ((bar (lyrion--progress-bar 150 600 32)))
    (should (= (length bar) 32))
    (should (= (cl-count ?━ bar) 8))
    (should (eq (get-text-property 0 'face bar) 'lyrion-elapsed))
    (should (eq (get-text-property 31 'face bar) 'lyrion-remaining)))
  (should (= (cl-count ?━ (lyrion--progress-bar 900 600 32)) 32))
  (should (= (cl-count ?━ (lyrion--progress-bar 10 nil 32)) 0)))

;;; Covers

(ert-deftest lyrion-test/cover-path ()
  (should (equal (lyrion--cover-path '(("coverid" . "1fffecd1")))
                 "/music/1fffecd1/cover_1600x1600_o.jpg"))
  (should (equal (lyrion--cover-path '(("artwork_url" . "/imageproxy/x/image.jpg")))
                 "/imageproxy/x/image_1600x1600_o.jpg"))
  (should (equal (lyrion--cover-path '(("artwork_url" . "/plugins/a/icon.png")))
                 "/plugins/a/icon.png"))
  ;; A streamed track's cover id is a stand-in, so its artwork is used.
  (should (equal (lyrion--cover-path '(("remote" . "1")
                                       ("coverid" . "-94249756330192")
                                       ("artwork_url" . "/imageproxy/x/image.jpg")))
                 "/imageproxy/x/image_1600x1600_o.jpg"))
  ;; A remote artwork address would need TLS; it is left out for now.
  (should-not (lyrion--cover-path '(("artwork_url" . "https://example.com/a.jpg"))))
  (should-not (lyrion--cover-path nil)))

(ert-deftest lyrion-test/http-response ()
  (should (equal (lyrion--http-response "HTTP/1.1 200 OK\r\nA: b\r\n\r\nDATA")
                 '(200 . "DATA")))
  (should (equal (lyrion--http-response "HTTP/1.1 404 Not Found\r\n\r\n")
                 '(404 . "")))
  (should-not (lyrion--http-response "")))

(defun lyrion-test--get (path)
  "Fetch PATH from the fake web server and return what the callback got."
  (let ((answer 'waiting))
    (lyrion--http-get lyrion-host lyrion-http-port path
                      (lambda (response) (setq answer response)))
    (should (lyrion-test--wait (lambda () (not (eq answer 'waiting))) 3))
    answer))

(ert-deftest lyrion-test/http-get-fetches-the-body ()
  (lyrion-test--with-http "HTTP/1.0 200 OK\r\nContent-Type: image/png\r\n\r\nPNGDATA"
    (should (equal (lyrion-test--get "/music/1/cover.jpg") '(200 . "PNGDATA")))
    (should (equal lyrion-test--requests '("/music/1/cover.jpg")))))

(ert-deftest lyrion-test/http-get-passes-on-a-missing-page ()
  (lyrion-test--with-http "HTTP/1.0 404 Not Found\r\n\r\n"
    (should (equal (lyrion-test--get "/music/1/cover.jpg") '(404 . "")))))

(ert-deftest lyrion-test/http-get-gives-up-on-a-silent-server ()
  (lyrion-test--with-http nil
    (let ((lyrion--http-timeout 0.3))
      (should-not (lyrion-test--get "/music/1/cover.jpg")))))

(ert-deftest lyrion-test/http-get-gives-up-when-it-cannot-connect ()
  (cl-letf (((symbol-function 'open-network-stream)
             (lambda (&rest _) (signal 'file-error '("Name or service not known")))))
    (let ((answer 'waiting))
      (lyrion--http-get "nowhere" 80 "/" (lambda (response) (setq answer response)))
      (should-not answer))))

(ert-deftest lyrion-test/a-cover-is-fetched-once-per-track ()
  (let ((fetched nil))
    (cl-letf (((symbol-function 'lyrion--http-get)
               (lambda (_host _port path _callback &rest _) (push path fetched))))
      (with-temp-buffer
        (lyrion--handle-status '(("title" . "One") ("coverid" . "a")))
        (lyrion--handle-status '(("title" . "One") ("coverid" . "a")
                                 ("mode" . "pause")))
        (lyrion--handle-status '(("title" . "Two") ("coverid" . "b")))
        (should (equal (nreverse fetched)
                       '("/music/a/cover_1600x1600_o.jpg"
                         "/music/b/cover_1600x1600_o.jpg")))))))

(ert-deftest lyrion-test/a-late-cover-for-another-track-is-dropped ()
  (let ((callbacks nil))
    (cl-letf (((symbol-function 'lyrion--http-get)
               (lambda (_host _port path callback &rest _)
                 (push (cons path callback) callbacks)))
              ((symbol-function 'lyrion--image) (lambda (data) data)))
      (with-temp-buffer
        (lyrion--handle-status '(("title" . "One") ("coverid" . "a")))
        (lyrion--handle-status '(("title" . "Two") ("coverid" . "b")))
        ;; The first track's cover arrives after the second track began.
        (funcall (cdr (assoc "/music/a/cover_1600x1600_o.jpg" callbacks))
                 '(200 . "A"))
        (should (equal lyrion--cover '("/music/b/cover_1600x1600_o.jpg")))
        (funcall (cdr (assoc "/music/b/cover_1600x1600_o.jpg" callbacks))
                 '(200 . "B"))
        (should (equal lyrion--cover '("/music/b/cover_1600x1600_o.jpg" . "B")))))))

(ert-deftest lyrion-test/the-last-cover-stays-until-the-next-arrives ()
  "Between albums the card keeps the last cover, rather than closing up."
  (let ((callbacks nil))
    (cl-letf (((symbol-function 'lyrion--http-get)
               (lambda (_host _port path callback &rest _)
                 (push (cons path callback) callbacks)))
              ((symbol-function 'lyrion--image) (lambda (data) data)))
      (with-temp-buffer
        (lyrion--handle-status '(("title" . "One") ("coverid" . "a")))
        (funcall (cdr (assoc "/music/a/cover_1600x1600_o.jpg" callbacks))
                 '(200 . "A"))
        (lyrion--handle-status '(("title" . "Two") ("coverid" . "b")))
        (should (equal lyrion--cover '("/music/b/cover_1600x1600_o.jpg" . "A")))
        (funcall (cdr (assoc "/music/b/cover_1600x1600_o.jpg" callbacks))
                 '(200 . "B"))
        (should (equal lyrion--cover '("/music/b/cover_1600x1600_o.jpg" . "B")))
        ;; A cover that cannot be fetched takes the last one with it.
        (lyrion--handle-status '(("title" . "Three") ("coverid" . "c")))
        (funcall (cdr (assoc "/music/c/cover_1600x1600_o.jpg" callbacks))
                 '(404 . ""))
        (should (equal lyrion--cover '("/music/c/cover_1600x1600_o.jpg")))
        ;; A track with no cover clears it at once.
        (lyrion--handle-status '(("title" . "Four") ("coverid" . "d")))
        (lyrion--handle-status '(("title" . "Five")))
        (should-not lyrion--cover)))))

;;; Drawing

(ert-deftest lyrion-test/render-shows-the-track ()
  (with-temp-buffer
    (setq lyrion--status '(("player_name" . "WiiM Ultra") ("mode" . "play")
                           ("time" . "60") ("duration" . "600")
                           ("title" . "Für Alina") ("artist" . "Arvo Pärt")
                           ("album" . "Alina") ("year" . "1999"))
          lyrion--status-time (float-time))
    (lyrion--render)
    (let ((text (buffer-string)))
      (dolist (expected '("Für Alina" "Arvo Pärt" "Alina · 1999" " 1:00"
                          "10:00" "WiiM Ultra"))
        (should (string-search expected text)))
      (should-not (string-search "paused" text)))
    (should (eq (get-text-property (1+ (string-search "Für" (buffer-string)))
                                   'face)
                'lyrion-title))))

(ert-deftest lyrion-test/render-shows-up-next ()
  (with-temp-buffer
    (let ((two '(("playlist index" . "0") ("title" . "Für Alina")
                 ("artist" . "Arvo Pärt")
                 ("playlist index" . "1") ("title" . "Spiegel im Spiegel")
                 ("artist" . "Arvo Pärt"))))
      (setq lyrion--status two)
      (lyrion--render)
      ;; The artist is left out when it is the one playing.
      (should (string-search "Up next  Spiegel im Spiegel  >\n" (buffer-string)))
      ;; The title stands out from the label, and so does the key.
      (let ((text (buffer-string)))
        (should (eq (get-text-property (1+ (string-search "Spiegel" text))
                                       'face)
                    'lyrion-next))
        (should (eq (get-text-property (1+ (string-search ">" text)) 'face)
                    'lyrion-next-key)))
      ;; It comes after the player's state, a blank line apart.
      (should (string-match-p "stopped\n\n.*Up next  " (buffer-string)))
      (setq lyrion--status
            (append (butlast two) '(("artist" . "Al Green"))))
      (lyrion--render)
      (should (string-search "Up next  Spiegel im Spiegel · Al Green  >"
                             (buffer-string)))
      ;; Repeating the track, or at the end of the queue, nothing is next.
      (setq lyrion--status (cons '("playlist repeat" . "1") two))
      (lyrion--render)
      (should-not (string-search "Up next" (buffer-string)))
      (setq lyrion--status (seq-take two 3))
      (lyrion--render)
      (should-not (string-search "Up next" (buffer-string))))))

(ert-deftest lyrion-test/render-numbers-the-track ()
  (with-temp-buffer
    (setq lyrion--status '(("title" . "Für Alina") ("tracknum" . "3")
                           ("disc" . "1") ("disccount" . "1")))
    (lyrion--render)
    (should (string-search "3. Für Alina" (buffer-string)))
    (should (eq (get-text-property (1+ (string-search "3." (buffer-string)))
                                   'face)
                'lyrion-track-number))
    (setq lyrion--status '(("title" . "Für Alina") ("tracknum" . "3")
                           ("disc" . "2") ("disccount" . "2")))
    (lyrion--render)
    (should (string-search "2-3. Für Alina" (buffer-string)))
    (setq lyrion--status '(("title" . "Für Alina") ("tracknum" . "0")))
    (lyrion--render)
    (should-not (string-search "0. " (buffer-string)))))

(ert-deftest lyrion-test/render-states-a-paused-or-lost-player ()
  (with-temp-buffer
    (setq lyrion--status '(("player_name" . "WiiM Ultra") ("mode" . "pause")
                           ("player_connected" . "0") ("title" . "Für Alina")))
    (lyrion--render)
    (should (string-search "WiiM Ultra · paused · not connected" (buffer-string)))))

(ert-deftest lyrion-test/render-leaves-out-an-unknown-year ()
  (with-temp-buffer
    (setq lyrion--status '(("title" . "T") ("album" . "Alina") ("year" . "0")))
    (lyrion--render)
    (should (string-search "Alina\n" (buffer-string)))
    (should-not (string-search "·  0" (buffer-string)))))

(ert-deftest lyrion-test/render-with-nothing-playing ()
  (with-temp-buffer
    (setq lyrion--status '(("player_name" . "WiiM Ultra") ("mode" . "stop")))
    (lyrion--render)
    (should (string-search "Nothing playing" (buffer-string)))
    (should (string-search "WiiM Ultra · stopped" (buffer-string)))))

(ert-deftest lyrion-test/render-shows-a-notice-alone ()
  (with-temp-buffer
    (setq lyrion--status '(("title" . "Für Alina"))
          lyrion--notice "Connecting to lms.lan")
    (lyrion--render)
    (should (string-search "Connecting to lms.lan" (buffer-string)))
    (should-not (string-search "Für Alina" (buffer-string)))))

(ert-deftest lyrion-test/lines-are-centred-on-the-window ()
  (let ((line (lyrion--centre "abcd")))
    (should (string-suffix-p "abcd\n" line))
    (should (equal (car (get-text-property 0 'display line)) 'space))
    (should (eq (car (plist-get (cdr (get-text-property 0 'display line))
                                :align-to))
                '-))))

(defun lyrion-test--outside-progress ()
  "Return the buffer's text with the progress line left out."
  (let ((text (buffer-string)) (start 0) (parts nil))
    (while (< start (length text))
      (let ((end (or (next-single-property-change start 'lyrion-progress text)
                     (length text))))
        (unless (get-text-property start 'lyrion-progress text)
          (push (substring-no-properties text start end) parts))
        (setq start end)))
    (apply #'concat (nreverse parts))))

(ert-deftest lyrion-test/update-progress-replaces-only-that-line ()
  (with-temp-buffer
    (setq lyrion--status '(("mode" . "play") ("time" . "60") ("duration" . "600")
                           ("title" . "Für Alina") ("player_name" . "WiiM Ultra"))
          lyrion--status-time (float-time))
    (lyrion--render)
    (let ((outside (lyrion-test--outside-progress)))
      (should (string-search " 1:00" (buffer-string)))
      (setq lyrion--status-time (- (float-time) 30))
      (lyrion--update-progress)
      (should (string-search " 1:30" (buffer-string)))
      (should-not (string-search " 1:00" (buffer-string)))
      (should (equal outside (lyrion-test--outside-progress)))
      (should (= 1 (how-many "10:00" (point-min) (point-max)))))))

;;; The connection

(ert-deftest lyrion-test/connect-chooses-the-player-and-subscribes ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (should (lyrion-test--shows "Arvo Pärt"))
      (should (lyrion-test--shows "WiiM Ultra"))
      (should (equal lyrion-test--received
                     '(("players" "0" "99")
                       ("02:00:00:00:00:01" "status" "-" "2" "tags:alcdyKtiq"
                        "subscribe:0")))))))

(ert-deftest lyrion-test/a-pushed-status-updates-the-player ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (process-send-string
       (car lyrion-test--clients)
       (lyrion-test--status-line "02:00:00:00:00:01" "mode:pause" "time:61"
                                 "duration:600" "title:Spiegel im Spiegel"
                                 "artist:Arvo Pärt"))
      (should (lyrion-test--wait
               (lambda () (lyrion-test--shows "Spiegel im Spiegel"))))
      (should (lyrion-test--shows "WiiM Ultra · paused")))))

(ert-deftest lyrion-test/another-players-status-is-ignored ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (process-send-string
       (car lyrion-test--clients)
       (lyrion-test--status-line "02:37:a6:bc:f5:7a" "mode:play"
                                 "title:Elsewhere"))
      (accept-process-output nil 0.2)
      (should-not (lyrion-test--shows "Elsewhere")))))

(ert-deftest lyrion-test/a-named-player-is-followed ()
  (lyrion-test--with-server #'lyrion-test--answer
    (let ((lyrion-player "emacs on EXAMPLE"))
      (lyrion-test--with-player
        (should (lyrion-test--wait (lambda () (= 2 (length lyrion-test--received)))))
        (should (equal (car (nth 1 lyrion-test--received)) "02:37:a6:bc:f5:7a"))))))

(ert-deftest lyrion-test/an-unknown-player-is-reported ()
  (lyrion-test--with-server #'lyrion-test--answer
    (let ((lyrion-player "Garage"))
      (lyrion-test--with-player
        (should (lyrion-test--wait
                 (lambda () (lyrion-test--shows "No player called Garage"))))))))

(ert-deftest lyrion-test/an-unknown-player-is-looked-for-again ()
  (lyrion-test--with-server #'lyrion-test--answer
    (let ((lyrion-player "Garage")
          (lyrion-reconnect-delay 0.2))
      (lyrion-test--with-player
        (should (lyrion-test--wait
                 (lambda () (= 2 (cl-count "players" lyrion-test--received
                                           :key #'car :test #'equal)))))))))

(ert-deftest lyrion-test/a-dropped-connection-is-made-again ()
  (lyrion-test--with-server #'lyrion-test--answer
    (let ((lyrion-reconnect-delay 0.2))
      (lyrion-test--with-player
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
        (delete-process (car lyrion-test--clients))
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "trying again"))))
        (should (lyrion-test--wait
                 (lambda () (= 2 (cl-count "players" lyrion-test--received
                                           :key #'car :test #'equal)))))
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))))))

(ert-deftest lyrion-test/an-unreachable-server-is-tried-again ()
  (let* ((port (let ((probe (make-network-process
                             :name "lyrion-probe" :server t :host "127.0.0.1"
                             :service t :family 'ipv4 :noquery t)))
                 (prog1 (process-contact probe :service) (delete-process probe))))
         (lyrion-host "127.0.0.1")
         (lyrion-cli-port port)
         (lyrion-reconnect-delay 30))
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "trying again"))))
      (should (timerp lyrion--retry)))))

(ert-deftest lyrion-test/a-connection-that-cannot-start-is-tried-again ()
  (lyrion-test--with-server #'lyrion-test--answer
    (let* ((lyrion-reconnect-delay 0.2)
           (connect (symbol-function 'make-network-process))
           (failed nil))
      ;; The first attempt fails at once, as an unknown host can.
      (cl-letf (((symbol-function 'make-network-process)
                 (lambda (&rest args)
                   (if failed
                       (apply connect args)
                     (setq failed t)
                     (signal 'file-error '("make client process failed"
                                           "Name or service not known"))))))
        (lyrion-test--with-player
          (should (lyrion-test--shows "Cannot reach 127.0.0.1"))
          (should (timerp lyrion--retry))
          (should (lyrion-test--wait
                   (lambda () (lyrion-test--shows "Für Alina")))))))))

(ert-deftest lyrion-test/asking-for-the-players-again-replaces-a-waiting-ask ()
  (with-temp-buffer
    (lyrion-mode)
    (lyrion--ask-players-later 30)
    (let ((first lyrion--retry))
      (lyrion--ask-players-later 30)
      (should-not (memq first timer-list))
      (should (memq lyrion--retry timer-list))
      (cancel-timer lyrion--retry))))

(ert-deftest lyrion-test/killing-the-buffer-closes-everything ()
  (lyrion-test--with-server #'lyrion-test--answer
    (let ((buffer (get-buffer-create lyrion-buffer-name))
          process timer)
      (with-current-buffer buffer
        (lyrion-mode)
        (lyrion--connect buffer)
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
        (setq process lyrion--process
              timer lyrion--timer))
      (kill-buffer buffer)
      (should-not (process-live-p process))
      (should-not (memq timer timer-list)))))

(ert-deftest lyrion-test/reconnect-chooses-the-player-afresh ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (let ((old lyrion--process))
        (lyrion-reconnect)
        (should-not (process-live-p old))
        (should (lyrion-test--wait
                 (lambda () (= 2 (cl-count "players" lyrion-test--received
                                           :key #'car :test #'equal)))))
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
        ;; The old connection's end does not start a retry of its own.
        (should-not lyrion--retry)))))

;;; Playing and pausing

(ert-deftest lyrion-test/space-plays-and-pauses-in-both-panes ()
  (should (eq (lookup-key lyrion-mode-map (kbd "SPC")) #'lyrion-play-pause))
  (should (eq (lookup-key lyrion-lyrics-mode-map (kbd "SPC")) #'lyrion-play-pause)))

(defun lyrion-test--commands-after-subscribing ()
  "Return the commands the fake server got after the status subscription."
  (nthcdr 2 lyrion-test--received))

(ert-deftest lyrion-test/play-pause-toggles-a-playing-player ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      ;; From any buffer: the lyrics pane uses it too.
      (with-temp-buffer (lyrion-play-pause))
      (should (lyrion-test--wait
               (lambda () (lyrion-test--commands-after-subscribing))))
      (should (equal (lyrion-test--commands-after-subscribing)
                     '(("02:00:00:00:00:01" "pause")))))))

(ert-deftest lyrion-test/play-pause-plays-a-stopped-player ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (process-send-string
       (car lyrion-test--clients)
       (lyrion-test--status-line "02:00:00:00:00:01" "mode:stop" "title:Für Alina"))
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "· stopped"))))
      (lyrion-play-pause)
      (should (lyrion-test--wait
               (lambda () (lyrion-test--commands-after-subscribing))))
      (should (equal (lyrion-test--commands-after-subscribing)
                     '(("02:00:00:00:00:01" "play")))))))

(ert-deftest lyrion-test/play-pause-needs-a-player ()
  (should-error (lyrion-play-pause) :type 'user-error)
  (let ((buffer (get-buffer-create lyrion-buffer-name)))
    (unwind-protect
        (with-current-buffer buffer
          ;; A player was chosen, but the connection has since gone.
          (setq lyrion--player '(("playerid" . "02:00:00:00:00:01")))
          (should-error (lyrion-play-pause) :type 'user-error))
      (kill-buffer buffer))))

;;; Searching the library

(defmacro lyrion-test--with-fresh-library (&rest body)
  "Run BODY with nothing listed or built yet this session."
  (declare (indent 0))
  `(let ((lyrion--library nil)
         (lyrion--library-pending nil)
         (lyrion--candidate-cache nil))
     ,@body))

(defun lyrion-test--load (kind)
  "List KIND from the fake server and return what the callback got."
  (let ((answer 'waiting))
    (lyrion--library-load kind (lambda (items) (setq answer items)))
    (should (lyrion-test--wait (lambda () (not (eq answer 'waiting))) 3))
    answer))

(defun lyrion-test--queries ()
  "Return the listing commands the fake server received, by name."
  (seq-filter (lambda (name)
                (member name '("titles" "albums" "artists" "works" "genres"
                               "playlists")))
              (mapcar #'car lyrion-test--received)))

(ert-deftest lyrion-test/group-items-by-their-first-field ()
  (should (equal (lyrion--group '("albums" "0" "9" "tags:lay" "count:2"
                                  "id:31" "album:Alina" "id:32" "album:Tabula Rasa")
                                "id")
                 '((("id" . "31") ("album" . "Alina"))
                   (("id" . "32") ("album" . "Tabula Rasa"))))))

(ert-deftest lyrion-test/candidate-texts-carry-dimmed-details ()
  (let ((track (lyrion--candidate-text
                'track '(("title" . "Für Alina") ("artist" . "Arvo Pärt")
                         ("album" . "Alina")))))
    (should (equal (substring-no-properties track) "Für Alina  Arvo Pärt · Alina"))
    (should-not (get-text-property 0 'face track))
    (should (eq (get-text-property (- (length track) 1) 'face track)
                'lyrion-search-detail))
    ;; The details are drawn against the right edge, one column clear of it.
    (should (equal (get-text-property (length "Für Alina ") 'display track)
                   `(space :align-to (- right ,(1+ (string-width
                                                    "Arvo Pärt · Alina")))))))
  (should (equal (substring-no-properties
                  (lyrion--candidate-text 'release '(("album" . "Alina")
                                                     ("artist" . "Arvo Pärt")
                                                     ("year" . "0"))))
                 "Alina  Arvo Pärt"))
  (should (equal (lyrion--candidate-text 'artist '(("artist" . "Arvo Pärt")))
                 "Arvo Pärt")))

(ert-deftest lyrion-test/work-texts-show-the-composer-and-release ()
  (should (equal (substring-no-properties
                  (lyrion--candidate-text 'work '(("work" . "China Gates")
                                                  ("composer" . "John Adams")
                                                  ("album" . "Must the Devil")
                                                  ("artist" . "Yuja Wang"))))
                 "China Gates  John Adams · Must the Devil"))
  (should (equal (substring-no-properties
                  (lyrion--candidate-text 'work '(("work" . "Für Alina")
                                                  ("composer" . "Arvo Pärt")
                                                  ("album" . "Alina")
                                                  ("artist" . "Arvo Pärt"))))
                 "Für Alina  Arvo Pärt · Alina")))

(ert-deftest lyrion-test/works-come-as-one-recording-per-release ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (let ((recordings (cdar (lyrion--library-items '(work)))))
        (should (equal (mapcar (lambda (item)
                                 (mapcar (lambda (key) (lyrion--get item key))
                                         '("work" "album_id" "album" "artist")))
                               recordings)
                       '(("Für Alina" "31" "Alina" "Arvo Pärt")
                         ("Spiegel im Spiegel" "31" "Alina" "Arvo Pärt")
                         ("Spiegel im Spiegel" "32" "Let's Stay Together"
                          "Al Green"))))))))

(ert-deftest lyrion-test/candidates-tell-twins-apart-and-are-kept ()
  (lyrion-test--with-fresh-library
    (let* ((items '((("id" . "502") ("title" . "Spiegel im Spiegel"))
                    (("id" . "503") ("title" . "Spiegel im Spiegel"))))
           (set (lyrion--candidates 'track items)))
      (should (equal (mapcar #'substring-no-properties (car set))
                     '("Spiegel im Spiegel" "Spiegel im Spiegel (2)")))
      (should (equal (gethash "Spiegel im Spiegel (2)" (cdr set))
                     (cons 'track (cadr items))))
      (should (eq set (lyrion--candidates 'track items))))))

(ert-deftest lyrion-test/icons-fall-back-without-nerd-fonts ()
  (cl-letf (((symbol-function 'nerd-icons-mdicon) nil))
    (fmakunbound 'nerd-icons-mdicon)
    (let ((icon (lyrion--icon 'release)))
      (should (equal (substring-no-properties icon) "◉"))
      (should (eq (get-text-property 0 'face icon) 'lyrion-icon-release)))))

(ert-deftest lyrion-test/icons-use-nerd-fonts-when-they-can ()
  (cl-letf (((symbol-function 'nerd-icons-mdicon)
             (lambda (name &rest arguments) (format "%s %S" name arguments))))
    (should (equal (lyrion--icon 'artist)
                   "nf-md-account_music (:face lyrion-icon-artist)"))))

(ert-deftest lyrion-test/query-reads-a-long-reply ()
  (let ((fields (mapcar (lambda (n) (format "id:%d" n)) (number-sequence 1 20000))))
    (lyrion-test--with-server
        (lambda (words) (lyrion--encode (append words fields)))
      (let ((answer 'waiting))
        (lyrion--query '("titles" "0" "100000") (lambda (reply) (setq answer reply)))
        (should (lyrion-test--wait (lambda () (not (eq answer 'waiting)))))
        (should (= (length answer) 20003))
        (should (equal (car (last answer)) "id:20000"))))))

(ert-deftest lyrion-test/query-gives-up-on-silence ()
  (lyrion-test--with-server #'ignore
    (let ((answer 'waiting)
          (lyrion--query-timeout 0.3))
      (lyrion--query '("titles") (lambda (reply) (setq answer reply)))
      (should (lyrion-test--wait (lambda () (not (eq answer 'waiting))) 3))
      (should-not answer))))

(ert-deftest lyrion-test/query-gives-up-when-it-cannot-connect ()
  (cl-letf (((symbol-function 'make-network-process)
             (lambda (&rest _) (signal 'file-error '("Name or service not known")))))
    (let ((answer 'waiting))
      (lyrion--query '("titles") (lambda (reply) (setq answer reply)))
      (should-not answer))))

(ert-deftest lyrion-test/a-short-answer-is-waited-for-briefly ()
  "The players and the queue wait the short time, not the library's long one."
  (lyrion-test--with-server #'ignore
    (let ((lyrion--quick-timeout 0.3)
          (lyrion--query-timeout 30)
          (start (float-time)))
      (should-error (lyrion--query-wait '("players" "0" "99")) :type 'user-error)
      (should (< (- (float-time) start) 3)))))

(ert-deftest lyrion-test/qobuz-is-waited-for-longer ()
  "Qobuz is asked through the server, so its searches wait longer."
  (let ((timeouts nil))
    (cl-letf (((symbol-function 'lyrion--query)
               (lambda (words callback &optional timeout)
                 (push timeout timeouts)
                 (funcall callback (append words '("count:0")))))
              ((symbol-function 'lyrion--player-id) (lambda () "02:00")))
      (lyrion--qobuz-search "brahms" '(qobuz-release qobuz-track))
      (lyrion--qobuz-artist-releases '(("id" . "0.0_brahms.0.1.0")))
      (should (equal timeouts (make-list 3 lyrion--qobuz-timeout)))
      (setq timeouts nil)
      (lyrion--query-wait '("players" "0" "99"))
      (should (equal timeouts (list lyrion--quick-timeout))))))

(ert-deftest lyrion-test/an-unanswered-qobuz-search-is-reported ()
  (cl-letf (((symbol-function 'lyrion--query)
             (lambda (words callback &optional _timeout)
               ;; The track search is answered, the release search is not.
               (funcall callback (and (string-search "item_id:0.0.2" (nth 5 words))
                                      (append words '("count:0"))))))
            ((symbol-function 'lyrion--player-id) (lambda () "02:00")))
    (should-error (lyrion--qobuz-search "brahms" '(qobuz-release qobuz-track))
                  :type 'user-error)))

(ert-deftest lyrion-test/a-lost-connection-is-not-waited-for ()
  "With the player's connection down, nothing is sent and nothing waited for."
  (let ((asked nil)
        (buffer (get-buffer-create lyrion-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'lyrion--query)
                   (lambda (words &rest _) (push words asked))))
          (with-current-buffer buffer
            (setq lyrion--process nil))
          (should (lyrion--disconnected-p))
          (let ((lyrion--library nil)
                (lyrion--library-pending nil)
                (lyrion--plugins-found nil))
            (dolist (wait (list (lambda () (lyrion--query-wait '("players")))
                                (lambda () (lyrion--query-all '(("a") ("b"))))
                                (lambda () (lyrion--library-items '(track)))))
              (should (equal (cadr (should-error (funcall wait)
                                                 :type 'user-error))
                             (format "Not connected to %s" lyrion-host))))
            ;; The search menu opens without asking about plugins.
            (lyrion--check-plugins)
            (should-not lyrion--plugins-found))
          (should-not asked))
      (kill-buffer buffer))))

(ert-deftest lyrion-test/a-long-wait-is-said ()
  "A wait that lasts says what it is for, and how to give up."
  (let ((said nil)
        (lyrion-host "lms.lan")
        (lyrion--wait-notice 0.1))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (push (and format (apply #'format format args)) said))))
      (let ((until (+ (float-time) 0.3)))
        (lyrion--wait (lambda () (> (float-time) until))))
      (should (equal (reverse said)
                     (list (format "Waiting for lms.lan... (%s gives up)"
                                   (substitute-command-keys "\\[keyboard-quit]"))
                           nil)))
      ;; A wait over at once says nothing.
      (setq said nil)
      (lyrion--wait (lambda () t))
      (should-not said))))

(ert-deftest lyrion-test/library-is-listed-once-a-session ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (let ((items (lyrion-test--load 'release)))
        (should (equal (mapcar (lambda (item) (lyrion--get item "album")) items)
                       '("Alina" "Let's Stay Together"))))
      (lyrion-test--load 'release)
      (should (equal (lyrion-test--queries) '("albums")))
      (should (equal (car lyrion-test--received)
                     '("albums" "0" "100000" "tags:lay"))))))

(ert-deftest lyrion-test/library-waiters-share-one-listing ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (let ((got nil))
        (lyrion--library-load 'artist (lambda (items) (push (length items) got)))
        (lyrion--library-load 'artist (lambda (items) (push (length items) got)))
        (should (lyrion-test--wait (lambda () (= 2 (length got)))))
        (should (equal got '(2 2)))
        (should (equal (lyrion-test--queries) '("artists")))))))

(ert-deftest lyrion-test/library-failure-is-not-kept ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'ignore
      (let ((lyrion--query-timeout 0.3))
        (should-not (lyrion-test--load 'track))
        (should-not lyrion--library)
        (should-error (lyrion--library-items '(track)) :type 'user-error)))))

(ert-deftest lyrion-test/search-menu-has-its-keys ()
  (let ((layout (format "%S" (get 'lyrion-search-menu 'transient--layout))))
    (dolist (suffix '(("\"t\"" lyrion-search-tracks)
                      ("\"r\"" lyrion-search-releases)
                      ("\"a\"" lyrion-search-album-artists)
                      ("\"w\"" lyrion-search-works)
                      ("\"g\"" lyrion-search-genres)
                      ("\"p\"" lyrion-search-playlists)
                      ("\"P\"" lyrion-radio-paradise)
                      ("\"/\"" lyrion-search-all)))
      (should (string-search (car suffix) layout))
      (should (string-search (symbol-name (cadr suffix)) layout))))
  (should (eq (lookup-key lyrion-mode-map (kbd "/")) #'lyrion-search))
  (should (eq (lookup-key lyrion-lyrics-mode-map (kbd "/")) #'lyrion-search)))

(ert-deftest lyrion-test/search-menu-keeps-playback-keys ()
  "Playback keys work in the search menu, and keep it open."
  (let ((suffixes (lyrion-test--suffixes
                   (get 'lyrion-search-menu 'transient--layout))))
    (dolist (key '("SPC" "<" ">" "<left>" "<right>" "L"))
      (should (eq (cdr (assoc key suffixes))
                  (lookup-key lyrion-mode-map (kbd key)))))))

(ert-deftest lyrion-test/search-menu-opens ()
  "Its keys do not clash, which only opening it shows."
  (let ((lyrion--plugins-found '(("qobuz" . t) ("radioparadise" . t))))
    (unwind-protect
        (transient-setup 'lyrion-search-menu)
      (transient-quit-all))))

(ert-deftest lyrion-test/search-lists-the-library-as-the-menu-opens ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (let ((opened nil))
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (prefix &rest _) (setq opened prefix))))
          (lyrion-search))
        (should (eq opened 'lyrion-search-menu))
        (should (lyrion-test--wait (lambda () (= 6 (length lyrion--library)))))
        (should (equal (sort (lyrion-test--queries) #'string<)
                       '("albums" "artists" "genres" "playlists" "titles"
                         "works")))))))

(defvar lyrion-test--table nil
  "The completion table the last stubbed `completing-read' was given.")

(defmacro lyrion-test--choosing (prefix &rest body)
  "Run BODY with `completing-read' choosing the first result starting PREFIX."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (_prompt table &rest _)
                (setq lyrion-test--table table)
                (substring-no-properties
                 (seq-find (lambda (text) (string-prefix-p ,prefix text))
                           (all-completions "" table))))))
     ,@body))

(ert-deftest lyrion-test/read-item-shows-icons-and-groups-everything ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--choosing "Spiegel im Spiegel  Arvo Pärt · Alina (2)"
        (should (equal (lyrion--read-item '(artist release track) "Search: ")
                       (cons 'track '(("id" . "503") ("title" . "Spiegel im Spiegel")
                                      ("artist" . "Arvo Pärt")
                                      ("album" . "Alina"))))))
      (let* ((metadata (cdr (funcall lyrion-test--table "" nil 'metadata)))
             (texts (all-completions "" lyrion-test--table))
             (affix (alist-get 'affixation-function metadata))
             (group (alist-get 'group-function metadata)))
        (should (eq (alist-get 'category metadata) 'lyrion-item))
        (should (= (length texts) 8))
        ;; Album artists first, then releases, then tracks.
        (should (equal (mapcar (lambda (text) (funcall group text nil)) texts)
                       '("Album artists" "Album artists" "Releases" "Releases"
                         "Tracks" "Tracks" "Tracks" "Tracks")))
        (should (equal (funcall group (car texts) t) (car texts)))
        (should (equal (substring-no-properties
                        (nth 1 (car (funcall affix (last texts)))))
                       (concat (substring-no-properties (lyrion--icon 'track))
                               " ")))))))

(defvar vertico-sort-function)
(defvar prescient-sort-length-enable)
(defvar prescient--history)
(defvar prescient--frequency)

(defun lyrion-test--search-metadata (kinds prefix)
  "Read an item of KINDS choosing PREFIX, and return the table's metadata."
  (lyrion-test--choosing prefix
    (lyrion--read-item kinds "Search"))
  (cdr (funcall lyrion-test--table "" nil 'metadata)))

(defmacro lyrion-test--with-fake-prescient (recent &rest body)
  "Run BODY with a Prescient-like UI sort that remembers RECENT, a prefix.
Like Prescient, it puts what it remembers first, and would sort the rest
by length unless told not to."
  (declare (indent 1))
  `(let ((vertico-sort-function 'prescient-completion-sort)
         (prescient-sort-length-enable t))
     (cl-letf (((symbol-function 'prescient-sort) #'ignore)
               ((symbol-function 'lyrion--remembered-p)
                (lambda (text) (string-prefix-p ,recent text)))
               ((symbol-function 'prescient-completion-sort)
                (lambda (candidates)
                  (let* ((recent (seq-filter (lambda (text)
                                               (string-prefix-p ,recent text))
                                             candidates))
                         (rest (seq-difference candidates recent)))
                    (append recent
                            (if prescient-sort-length-enable
                                (seq-sort-by #'length #'< rest)
                              rest))))))
       ,@body)))

(ert-deftest lyrion-test/candidates-are-alphabetical ()
  "Results are kept in alphabetical order, regardless of case."
  (lyrion-test--with-fresh-library
    (should (equal (mapcar #'substring-no-properties
                           (car (lyrion--candidates
                                 'artist '((("id" . "1") ("artist" . "beta"))
                                           (("id" . "2") ("artist" . "Alpha"))
                                           (("id" . "3") ("artist" . "gamma"))))))
                   '("Alpha" "beta" "gamma")))))

(defun lyrion-test--sorted (sort group texts)
  "Sort TEXTS with SORT, and return each one's GROUP and name, in order."
  (mapcar (lambda (text)
            (cons (and group (funcall group text nil))
                  (car (split-string text "  "))))
          (funcall sort texts)))

(ert-deftest lyrion-test/recent-results-first-then-alphabetical ()
  "Prescient's memory of recent choices leads; its length rule does not."
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-fake-prescient "Spiegel"
        (let* ((metadata (lyrion-test--search-metadata '(track) "Für Alina"))
               (texts (all-completions "" lyrion-test--table)))
          (should (equal (lyrion-test--sorted
                          (alist-get 'display-sort-function metadata)
                          (alist-get 'group-function metadata)
                          texts)
                         '(("Recent" . "Spiegel im Spiegel")
                           ("Recent" . "Spiegel im Spiegel")
                           ("Tracks" . "Für Alina")
                           ("Tracks" . "Let's Stay Together")))))))))

(ert-deftest lyrion-test/recent-results-are-limited ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-fake-prescient "Spiegel"
        (let* ((lyrion-search-recent 1)
               (metadata (lyrion-test--search-metadata '(track) "Für Alina"))
               (texts (all-completions "" lyrion-test--table)))
          ;; The other Spiegel im Spiegel goes back to its place.
          (should (equal (lyrion-test--sorted
                          (alist-get 'display-sort-function metadata)
                          (alist-get 'group-function metadata)
                          texts)
                         '(("Recent" . "Spiegel im Spiegel")
                           ("Tracks" . "Für Alina")
                           ("Tracks" . "Let's Stay Together")
                           ("Tracks" . "Spiegel im Spiegel")))))))))

(ert-deftest lyrion-test/no-recent-results-leaves-the-alphabet ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-fake-prescient "Spiegel"
        (let* ((lyrion-search-recent 0)
               (metadata (lyrion-test--search-metadata '(track) "Für Alina"))
               (texts (all-completions "" lyrion-test--table)))
          (should-not (alist-get 'group-function metadata))
          (should (equal (funcall (alist-get 'display-sort-function metadata)
                                  texts)
                         texts)))))))

(ert-deftest lyrion-test/one-kind-is-not-grouped-with-nothing-recent ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-fake-prescient "Nothing like this"
        (should-not (alist-get 'group-function
                               (lyrion-test--search-metadata '(track)
                                                             "Für Alina")))))))

(ert-deftest lyrion-test/ordered-results-have-nothing-recent ()
  "Results in an order of their own, such as a search's relevance, keep it."
  (lyrion-test--with-fake-prescient "Zebra"
    (let ((sets (list (lyrion--candidates-of
                       'qobuz-release '((("album" . "Zebra")) (("album" . "Aardvark")))))))
      (lyrion-test--choosing "Zebra"
        (lyrion--read-item '(qobuz-release) "Release" sets))
      (let* ((metadata (cdr (funcall lyrion-test--table "" nil 'metadata)))
             (texts (all-completions "" lyrion-test--table)))
        (should-not (alist-get 'group-function metadata))
        (should (equal (funcall (alist-get 'display-sort-function metadata)
                                texts)
                       texts))))))

(ert-deftest lyrion-test/remembered-is-what-prescient-has-seen ()
  (let ((prescient--history (make-hash-table :test #'equal))
        (prescient--frequency (make-hash-table :test #'equal)))
    (puthash "Für Alina" 0 prescient--history)
    (puthash "Alina" 1.5 prescient--frequency)
    (should (lyrion--remembered-p (propertize "Für Alina" 'face 'bold)))
    (should (lyrion--remembered-p "Alina"))
    (should-not (lyrion--remembered-p "Spiegel im Spiegel"))))

(ert-deftest lyrion-test/another-ui-sort-gives-way-to-the-alphabet ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (let* ((sort (alist-get 'display-sort-function
                              (lyrion-test--search-metadata '(release) "Alina")))
             (texts (all-completions "" lyrion-test--table))
             (vertico-sort-function (lambda (candidates) (reverse candidates))))
        (should (equal (funcall sort texts) texts))))))

(ert-deftest lyrion-test/everything-keeps-its-groups-in-order ()
  "Recent ones, then album artists, then releases, then tracks."
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-fake-prescient "Let's Stay Together"
        (let* ((metadata (lyrion-test--search-metadata '(artist release track)
                                                       "Al Green"))
               (texts (all-completions "" lyrion-test--table)))
          (should (equal (lyrion-test--sorted
                          (alist-get 'display-sort-function metadata)
                          (alist-get 'group-function metadata)
                          texts)
                         '(("Recent" . "Let's Stay Together")
                           ("Recent" . "Let's Stay Together")
                           ("Album artists" . "Al Green")
                           ("Album artists" . "Arvo Pärt")
                           ("Releases" . "Alina")
                           ("Tracks" . "Für Alina")
                           ("Tracks" . "Spiegel im Spiegel")
                           ("Tracks" . "Spiegel im Spiegel")))))))))

(ert-deftest lyrion-test/read-item-of-one-kind-is-not-grouped ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--choosing "Alina"
        (should (equal (car (lyrion--read-item '(release) "Release: ")) 'release)))
      (should-not (alist-get 'group-function
                             (cdr (funcall lyrion-test--table "" nil 'metadata)))))))

;; Fitting results to the window

(defconst lyrion-test--clair
  (lyrion--with-detail "Clair de Lune" '("Debussy" "Suite bergamasque"))
  "A result 42 columns wide: a 13-column name and a 27-column detail.")

(ert-deftest lyrion-test/fit-leaves-a-result-that-fits ()
  (should (eq (lyrion--fit lyrion-test--clair 42) lyrion-test--clair)))

(ert-deftest lyrion-test/fit-shortens-the-detail-then-the-name ()
  (should (equal (lyrion--fit lyrion-test--clair 30)
                 "Clair de Lune  Debussy · Suit…"))
  (should (eq (get-text-property 29 'face (lyrion--fit lyrion-test--clair 30))
              'lyrion-search-detail))
  ;; The name keeps half.
  (should (equal (lyrion--fit lyrion-test--clair 20) "Clair de …  Debussy…"))
  ;; Too little room for any detail.
  (should (equal (lyrion--fit lyrion-test--clair 6) "Clair…"))
  (should (equal (lyrion--fit (lyrion--with-detail "Johann Sebastian Bach" nil)
                              10)
                 "Johann Se…")))

(ert-deftest lyrion-test/fit-at-an-offset-shows-the-result-from-there ()
  (should (equal (lyrion--fit lyrion-test--clair 30 0)
                 "Clair de Lune  Debussy · Suite"))
  (should (equal (lyrion--fit lyrion-test--clair 30 12)
                 "e  Debussy · Suite bergamasque")))

(ert-deftest lyrion-test/scrolling-waits-moves-waits-and-jumps-back ()
  (let ((lyrion--scroll-pause 1.5)
        (lyrion--scroll-speed 8))
    ;; 16 columns take 2 seconds, so each round takes 5.
    (should (equal (mapcar (lambda (elapsed) (lyrion--scroll-offset elapsed 16))
                           '(0 1.4 2.5 3.5 4.9 5.2 7.5))
                   '(0 0 8 16 16 0 8)))))

(defvar vertico--input)
(defvar vertico--index)
(defvar vertico--candidates)

(defmacro lyrion-test--in-vertico (width selected candidates &rest body)
  "Run BODY as if Vertico showed CANDIDATES, SELECTED, in WIDTH columns."
  (declare (indent 3))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'vertico--window-width) (lambda () ,width)))
       (let ((vertico--input t)
             (vertico--index ,selected)
             (vertico--candidates ,candidates))
         ,@body))))

(ert-deftest lyrion-test/a-fitted-result-keeps-its-group ()
  "Vertico asks for the group of a result as shown, shortened or not."
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--choosing "Alina"
        (lyrion--read-item '(release track) "Search"))
      (let* ((metadata (cdr (funcall lyrion-test--table "" nil 'metadata)))
             (affix (alist-get 'affixation-function metadata))
             (group (alist-get 'group-function metadata))
             (texts (all-completions "" lyrion-test--table))
             (track (car (last texts))))
        (lyrion-test--in-vertico 12 -1 texts
          (let ((shown (car (car (funcall affix (list track))))))
            (should (string-suffix-p "…" shown))
            (should (equal (funcall group shown nil) "Tracks"))
            (should (eq (funcall group shown t) shown))))))))

(ert-deftest lyrion-test/fit-to-window-scrolls-only-the-selected-result ()
  (let* ((short (lyrion--with-detail "Clair" nil))
         (texts (list lyrion-test--clair short
                      (format "%s (2)" lyrion-test--clair)))
         (affixed (mapcar (lambda (text) (list text "  " "")) texts))
         (lyrion-search-scroll t)
         (lyrion--scroll-pause 1.5)
         (lyrion--scroll-speed 8))
    ;; 33 columns leave 30 for the result.
    (lyrion-test--in-vertico 33 0 texts
      (should (equal (mapcar #'car (lyrion--fit-to-window affixed))
                     (list "Clair de Lune  Debussy · Suite" short
                           "Clair de Lune  Debussy · Suit…")))
      (should (equal (car lyrion--scroll) (car texts)))
      (should (equal (cddr lyrion--scroll) '(12 0)))
      ;; Selected a second past the pause, it has moved 8 columns.
      (setf (nth 1 lyrion--scroll) (- (float-time) 2.5))
      (should (equal (car (car (lyrion--fit-to-window affixed)))
                     " Lune  Debussy · Suite bergama"))
      (let ((lyrion-search-scroll nil))
        (should (equal (car (car (lyrion--fit-to-window affixed)))
                       "Clair de Lune  Debussy · Suit…"))
        (should-not lyrion--scroll)))
    ;; Without Vertico, results stay as they are.
    (should (equal (lyrion--fit-to-window affixed) affixed))))

(defun lyrion-test--search-sends-raw (command)
  "Run search COMMAND, with `completing-read' as it is, and return what it sent."
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-player
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
        (cl-letf (((symbol-function 'message) #'ignore))
          (funcall command))
        (should (lyrion-test--wait
                 (lambda () (= 2 (cl-count "02:00:00:00:00:01"
                                           (cddr lyrion-test--received)
                                           :key #'car :test #'equal)))))
        (mapcar #'cdr (seq-filter (lambda (words)
                                    (equal (car words) "02:00:00:00:00:01"))
                                  (cddr lyrion-test--received)))))))

(defun lyrion-test--search-sends (command prefix)
  "Run search COMMAND choosing PREFIX, and return the command it sent."
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-player
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
        (lyrion-test--choosing prefix
          (cl-letf (((symbol-function 'message) #'ignore))
            (funcall command)))
        (should (lyrion-test--wait
                 (lambda () (= 2 (cl-count "02:00:00:00:00:01"
                                           (cddr lyrion-test--received)
                                           :key #'car :test #'equal)))))
        (mapcar #'cdr (seq-filter (lambda (words)
                                    (equal (car words) "02:00:00:00:00:01"))
                                  (cddr lyrion-test--received)))))))

(defun lyrion-test--search-choosing (command choices)
  "Run search COMMAND, answering each prompt from CHOICES in turn.
Each choice is (PREFIX . KEY): the result starting PREFIX, accepted with
RET when KEY is nil, else as S-RET (`shuffle') or M-RET (`open') would.
Return (PROMPTS QUERIES SENT): the prompts shown, the queries sent on
connections of their own, and the commands sent for the player."
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-player
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
        (let ((prompts nil))
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'completing-read)
                     (lambda (prompt table &rest _)
                       (pcase-let ((`(,prefix . ,key) (pop choices)))
                         (push prompt prompts)
                         (pcase key
                           ('shuffle (setq lyrion--shuffle-chosen t))
                           ('open (setq lyrion--open-chosen t)))
                         (substring-no-properties
                          (seq-find (lambda (text) (string-prefix-p prefix text))
                                    (all-completions "" table)))))))
            (funcall command))
          (should (lyrion-test--wait
                   (lambda () (= 2 (cl-count "02:00:00:00:00:01"
                                             (cddr lyrion-test--received)
                                             :key #'car :test #'equal)))))
          (list (nreverse prompts)
                (seq-remove (lambda (words)
                              (member (car words)
                                      '("02:00:00:00:00:01" "players"
                                        "subscribe")))
                            lyrion-test--received)
                (mapcar #'cdr (seq-filter (lambda (words)
                                            (equal (car words)
                                                   "02:00:00:00:00:01"))
                                          (cddr lyrion-test--received)))))))))

(ert-deftest lyrion-test/an-opened-artist-offers-their-releases ()
  "M-RET on an album artist lists their releases, and RET then plays one."
  (pcase-let ((`(,prompts ,queries ,sent)
               (lyrion-test--search-choosing #'lyrion-search-album-artists
                                             '(("Arvo Pärt" . open)
                                               ("Alina")))))
    (should (member '("albums" "0" "100000" "artist_id:9" "sort:yearalbum"
                      "tags:lay")
                    queries))
    (should (equal (cadr prompts) "Release by Arvo Pärt (S-RET shuffles): "))
    (should (equal sent '(("playlist" "shuffle" "0")
                          ("playlistcontrol" "cmd:load" "album_id:31"))))))

(ert-deftest lyrion-test/an-opened-artist-release-can-be-shuffled ()
  (pcase-let ((`(,_ ,_ ,sent)
               (lyrion-test--search-choosing #'lyrion-search-album-artists
                                             '(("Arvo Pärt" . open)
                                               ("Alina" . shuffle)))))
    (should (equal sent '(("playlist" "shuffle" "1")
                          ("playlistcontrol" "cmd:load" "album_id:31"))))))

(ert-deftest lyrion-test/opening-anything-else-plays-it ()
  "M-RET on what is not an artist plays it, as RET does."
  (pcase-let ((`(,_ ,queries ,sent)
               (lyrion-test--search-choosing #'lyrion-search-releases
                                             '(("Alina" . open)))))
    (should-not (seq-find (lambda (words)
                            (seq-find (lambda (word)
                                        (string-prefix-p "artist_id:" word))
                                      words))
                          queries))
    (should (equal sent '(("playlist" "shuffle" "0")
                          ("playlistcontrol" "cmd:load" "album_id:31"))))))

(ert-deftest lyrion-test/the-prompt-names-the-keys ()
  "The prompt names M-RET only where there are artists to open."
  (let ((first-prompt (lambda (command choice)
                        (caar (lyrion-test--search-choosing
                               command (list (list choice)))))))
    (should (equal (funcall first-prompt #'lyrion-search-album-artists
                            "Al Green")
                   "Album artist (S-RET shuffles, M-RET opens an artist): "))
    (should (equal (funcall first-prompt #'lyrion-search-tracks "Für Alina")
                   "Track (S-RET shuffles): "))))

(ert-deftest lyrion-test/a-chosen-track-plays-alone ()
  (should (equal (lyrion-test--search-sends #'lyrion-search-tracks "Für Alina")
                 '(("playlist" "shuffle" "0")
                   ("playlistcontrol" "cmd:load" "track_id:501")))))

(ert-deftest lyrion-test/a-chosen-release-plays-whole ()
  (should (equal (lyrion-test--search-sends #'lyrion-search-releases "Alina")
                 '(("playlist" "shuffle" "0")
                   ("playlistcontrol" "cmd:load" "album_id:31")))))

(ert-deftest lyrion-test/a-chosen-artist-plays-everything-by-them ()
  (should (equal (lyrion-test--search-sends #'lyrion-search-album-artists "Al Green")
                 '(("playlist" "shuffle" "0")
                   ("playlistcontrol" "cmd:load" "artist_id:12")))))

(ert-deftest lyrion-test/a-chosen-genre-plays-everything-in-it ()
  (should (equal (lyrion-test--search-sends #'lyrion-search-genres "Soul")
                 '(("playlist" "shuffle" "0")
                   ("playlistcontrol" "cmd:load" "genre_id:4")))))

(ert-deftest lyrion-test/a-chosen-playlist-plays-it ()
  (should (equal (lyrion-test--search-sends #'lyrion-search-playlists
                                            "Sunday Morning")
                 '(("playlist" "shuffle" "0")
                   ("playlistcontrol" "cmd:load" "playlist_id:71")))))

(ert-deftest lyrion-test/a-chosen-work-plays-that-recording ()
  (should (equal (lyrion-test--search-sends #'lyrion-search-works
                                            "Spiegel im Spiegel  Arvo Pärt · Let's")
                 '(("playlist" "shuffle" "0")
                   ("playlistcontrol" "cmd:load" "work_id:8" "album_id:32")))))

(ert-deftest lyrion-test/everything-searches-every-kind ()
  (should (equal (lyrion-test--search-sends #'lyrion-search-all "Let's Stay Together  Al Green · 1972")
                 '(("playlist" "shuffle" "0")
                   ("playlistcontrol" "cmd:load" "album_id:32")))))

(ert-deftest lyrion-test/shift-return-shuffles-the-choice ()
  "S-RET accepts the result as RET would, and plays it shuffled."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _)
               ;; S-RET, pressed with the artist highlighted.
               (cl-letf (((symbol-function 'key-binding)
                          (lambda (&rest _) #'ignore)))
                 (lyrion-search-shuffle-exit))
               "Al Green")))
    (should (equal (lyrion-test--search-sends-raw #'lyrion-search-album-artists)
                   '(("playlist" "shuffle" "1")
                     ("playlistcontrol" "cmd:load" "artist_id:12"))))))

(ert-deftest lyrion-test/shift-return-is-bound-in-the-search ()
  (should (eq (lookup-key lyrion-search-minibuffer-map (kbd "S-<return>"))
              #'lyrion-search-shuffle-exit)))

(ert-deftest lyrion-test/shuffle-exit-accepts-as-return-does ()
  (let ((accepted nil)
        (lyrion--shuffle-chosen nil))
    (cl-letf (((symbol-function 'key-binding)
               (lambda (key &rest _)
                 (should (equal key (kbd "RET")))
                 (lambda () (interactive) (setq accepted t)))))
      (lyrion-search-shuffle-exit))
    (should accepted)
    (should lyrion--shuffle-chosen)))

(ert-deftest lyrion-test/search-prompt-mentions-shuffle ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (let ((prompt nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (given &rest _) (setq prompt given) "Alina")))
          (ignore-errors (lyrion--read-item '(release) "Release")))
        (should (string-search "S-RET" prompt))))))

(ert-deftest lyrion-test/reconnect-lists-the-library-anew ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-player
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
        (lyrion-test--load 'artist)
        (lyrion--candidates 'artist (alist-get 'artist lyrion--library))
        (lyrion-reconnect)
        (should-not lyrion--library)
        (should-not lyrion--candidate-cache)))))

;;; Moving through a track and the queue

(ert-deftest lyrion-test/transport-keys-in-both-panes ()
  (dolist (map (list lyrion-mode-map lyrion-lyrics-mode-map))
    (should (eq (lookup-key map (kbd "<")) #'lyrion-previous))
    (should (eq (lookup-key map (kbd ">")) #'lyrion-next))
    (should (eq (lookup-key map (kbd "<left>")) #'lyrion-skip-back))
    (should (eq (lookup-key map (kbd "<right>")) #'lyrion-skip-forward))
    (should (eq (lookup-key map (kbd "p")) #'lyrion-select-player))))

(defun lyrion-test--sends (command)
  "Run COMMAND against a connected player and return what it sent."
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (funcall command)
      (should (lyrion-test--wait (lambda () (lyrion-test--commands-after-subscribing))))
      (car (lyrion-test--commands-after-subscribing)))))

(ert-deftest lyrion-test/previous-and-next-jump-in-the-queue ()
  (should (equal (lyrion-test--sends #'lyrion-previous)
                 '("02:00:00:00:00:01" "playlist" "index" "-1")))
  (should (equal (lyrion-test--sends #'lyrion-next)
                 '("02:00:00:00:00:01" "playlist" "index" "+1"))))

(ert-deftest lyrion-test/arrows-skip-by-the-chosen-interval ()
  (should (equal (lyrion-test--sends #'lyrion-skip-forward)
                 '("02:00:00:00:00:01" "time" "+10")))
  (let ((lyrion-skip-seconds 30))
    (should (equal (lyrion-test--sends #'lyrion-skip-back)
                   '("02:00:00:00:00:01" "time" "-30")))))

;;; Shuffle and repeat

(ert-deftest lyrion-test/shuffle-and-repeat-keys-in-both-panes ()
  (dolist (map (list lyrion-mode-map lyrion-lyrics-mode-map))
    (should (eq (lookup-key map (kbd "S-<return>")) #'lyrion-shuffle))
    (should (eq (lookup-key map (kbd "S")) #'lyrion-shuffle-toggle))
    (should (eq (lookup-key map (kbd "O")) #'lyrion-play-in-order))
    (should-not (lookup-key map (kbd "P")))
    (should (eq (lookup-key map (kbd "R")) #'lyrion-repeat))))

(ert-deftest lyrion-test/shuffle-starts-a-new-order-from-the-top ()
  "Setting shuffle on, even when it is on, has the server shuffle again.
The server puts the track playing first, so play moves on to the second,
and a second shuffle then puts that one first, the old one among the rest."
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (lyrion-shuffle)
      (should (lyrion-test--wait
               (lambda () (= 3 (length (lyrion-test--commands-after-subscribing))))))
      (should (equal (lyrion-test--commands-after-subscribing)
                     '(("02:00:00:00:00:01" "playlist" "shuffle" "1")
                       ("02:00:00:00:00:01" "playlist" "index" "1")
                       ("02:00:00:00:00:01" "playlist" "shuffle" "1")))))))

(ert-deftest lyrion-test/play-in-order-starts-the-queue-again ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (lyrion-play-in-order)
      (should (lyrion-test--wait
               (lambda () (= 2 (length (lyrion-test--commands-after-subscribing))))))
      (should (equal (lyrion-test--commands-after-subscribing)
                     '(("02:00:00:00:00:01" "playlist" "shuffle" "0")
                       ("02:00:00:00:00:01" "playlist" "index" "0")))))))

(ert-deftest lyrion-test/shuffle-toggle-turns-it-on-when-off ()
  "The track playing carries on: only the shuffle mode is sent."
  (should (equal (lyrion-test--sends #'lyrion-shuffle-toggle)
                 '("02:00:00:00:00:01" "playlist" "shuffle" "1"))))

(ert-deftest lyrion-test/shuffle-toggle-turns-it-off-when-on ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (process-send-string
       (car lyrion-test--clients)
       (lyrion-test--status-line "02:00:00:00:00:01" "mode:play"
                                 "playlist shuffle:1" "title:Für Alina"))
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "· shuffle"))))
      (lyrion-shuffle-toggle)
      (should (lyrion-test--wait
               (lambda () (lyrion-test--commands-after-subscribing))))
      (should (equal (lyrion-test--commands-after-subscribing)
                     '(("02:00:00:00:00:01" "playlist" "shuffle" "0")))))))

(ert-deftest lyrion-test/repeat-goes-from-off-to-the-track ()
  (should (equal (lyrion-test--sends #'lyrion-repeat)
                 '("02:00:00:00:00:01" "playlist" "repeat" "1"))))

(ert-deftest lyrion-test/repeat-goes-from-all-back-to-off ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (process-send-string
       (car lyrion-test--clients)
       (lyrion-test--status-line "02:00:00:00:00:01" "mode:play"
                                 "playlist repeat:2" "title:Für Alina"))
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "· repeat all"))))
      (lyrion-repeat)
      (should (lyrion-test--wait
               (lambda () (lyrion-test--commands-after-subscribing))))
      (should (equal (lyrion-test--commands-after-subscribing)
                     '(("02:00:00:00:00:01" "playlist" "repeat" "0")))))))

(defun lyrion-test--crossfade-sends (type)
  "Press X on a player whose transition type is TYPE; return what it set."
  (let ((lyrion-test--transition-type type))
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-player
        (should (lyrion-test--wait (lambda () lyrion--transition)))
        (cl-letf (((symbol-function 'message) #'ignore))
          (lyrion-crossfade))
        (should (lyrion-test--wait
                 (lambda () (lyrion-test--commands-after-subscribing))))
        (accept-process-output nil 0.1)
        (lyrion-test--commands-after-subscribing)))))

(defun lyrion-test--suffixes (layout)
  "Return the suffixes in LAYOUT, a transient's, as (KEY . COMMAND)."
  (let ((found nil))
    (named-let walk ((part layout))
      (cond ((and (consp part) (keywordp (car part)) (plist-get part :key))
             (push (cons (plist-get part :key) (plist-get part :command))
                   found))
            ((consp part) (walk (car part)) (walk (cdr part)))
            ((vectorp part) (mapc #'walk part))))
    found))

(ert-deftest lyrion-test/dispatch-lists-every-key ()
  "The `?' menu offers every key of both panes, and runs the same command."
  (let ((suffixes (lyrion-test--suffixes
                   (get 'lyrion-dispatch 'transient--layout))))
    (dolist (map (list lyrion-mode-map lyrion-lyrics-mode-map))
      (should (eq (lookup-key map (kbd "?")) #'lyrion-dispatch))
      (map-keymap-internal
       (lambda (event command)
         ;; `q' is left to the panes: in the menu, C-g closes it.
         (unless (memq command '(lyrion-dispatch lyrion-quit quit-window))
           (should (eq (cdr (assoc (key-description (vector event)) suffixes))
                       command))))
       map))
    (should (eq (cdr (assoc "g" suffixes)) #'revert-buffer))
    ;; The lyrics share the player's keys, but `q' hides them alone.
    (should (eq (lookup-key lyrion-lyrics-mode-map (kbd "q")) #'quit-window))))

(ert-deftest lyrion-test/volume-keys-step-the-volume ()
  (let ((lyrion-volume-step 5))
    (should (equal (lyrion-test--sends #'lyrion-volume-up)
                   '("02:00:00:00:00:01" "mixer" "volume" "+5")))
    (should (equal (lyrion-test--sends #'lyrion-volume-down)
                   '("02:00:00:00:00:01" "mixer" "volume" "-5")))))

(ert-deftest lyrion-test/a-volume-change-is-said ()
  (with-temp-buffer
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments)
                   (push (apply #'format format arguments) said)))
                ((symbol-function 'lyrion--lyrics-follow) #'ignore))
        (dolist (volume '("40" "40" "45"))
          (lyrion--handle-status `(("player_name" . "WiiM Ultra")
                                   ("mixer volume" . ,volume)))))
      ;; Only the change is said: not the first status, nor the same again.
      (should (equal said '("Volume 45%"))))))

(ert-deftest lyrion-test/sleep-sets-the-timer ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (dolist (case '(("30 minutes" . "1800") ("Off" . "0")))
        (setq lyrion-test--received nil)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _)
                     (should (equal (all-completions "" table)
                                    (mapcar #'car lyrion--sleep-choices)))
                     (car case)))
                  ((symbol-function 'message) #'ignore))
          (lyrion-sleep))
        (should (lyrion-test--wait (lambda () lyrion-test--received)))
        (should (equal lyrion-test--received
                       `(("02:00:00:00:00:01" "sleep" ,(cdr case)))))))))

(ert-deftest lyrion-test/render-states-the-sleep-time ()
  (with-temp-buffer
    (let ((now (float-time)))
      (setq lyrion--status '(("player_name" . "WiiM Ultra") ("mode" . "play")
                             ("title" . "Für Alina") ("will_sleep_in" . "1800"))
            lyrion--status-time now)
      (lyrion--render)
      (should (string-search (concat "WiiM Ultra · sleeps at "
                                     (format-time-string "%H:%M" (+ now 1800)))
                             (buffer-string))))))

(ert-deftest lyrion-test/copy-track-gives-artist-release-and-title ()
  (let ((buffer (get-buffer-create lyrion-buffer-name))
        (kill-ring nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore))
          (with-current-buffer buffer
            (setq-local lyrion--status '(("title" . "Für Alina")
                                         ("artist" . "Arvo Pärt")
                                         ("album" . "Alina"))))
          (lyrion-copy-track)
          (should (equal (car kill-ring) "Arvo Pärt - Alina - Für Alina"))
          (with-current-buffer buffer
            (setq-local lyrion--status '(("title" . "Für Alina")
                                         ("artist" . "Arvo Pärt"))))
          (lyrion-copy-track)
          (should (equal (car kill-ring) "Arvo Pärt - Für Alina"))
          (with-current-buffer buffer
            (setq-local lyrion--status '(("current_title" . "Radio Paradise"))))
          (lyrion-copy-track)
          (should (equal (car kill-ring) "Radio Paradise"))
          (with-current-buffer buffer
            (setq-local lyrion--status nil))
          (should-error (lyrion-copy-track) :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest lyrion-test/dispatch-labels-give-the-state ()
  (let ((buffer (get-buffer-create lyrion-buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local lyrion--status '(("playlist repeat" . "2")))
            (setq-local lyrion--transition '(("transitionType" . "1")
                                             ("transitionSmart" . "1"))))
          (should (equal (lyrion--repeat-description) "Repeat (all)"))
          (should (equal (lyrion--crossfade-description) "Crossfade (smart)"))
          (with-current-buffer buffer
            (setq-local lyrion--transition '(("transitionType" . "0"))))
          (should (equal (lyrion--crossfade-description) "Crossfade (off)")))
      (kill-buffer buffer)))
  (should (equal (lyrion--repeat-description) "Repeat (off)"))
  (should (equal (lyrion--crossfade-description) "Crossfade")))

(ert-deftest lyrion-test/queue-jumps-to-the-track-chosen ()
  "The queue shows in play order, the track playing marked, and choosing a
track plays it where it is, without loading the queue again."
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (let ((prompt nil) (texts nil) (metadata nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (given table &rest _)
                     (setq prompt given
                           texts (all-completions "" table)
                           metadata (cdr (funcall table "" nil 'metadata)))
                     (car (last texts)))))
          (lyrion-queue))
        (should (equal prompt "Queue (playing 2 of 3): "))
        (should (equal (mapcar #'substring-no-properties texts)
                       '("Für Alina  Arvo Pärt · Alina"
                         "Spiegel im Spiegel  Arvo Pärt · Alina"
                         "Let's Stay Together  Al Green · Let's Stay Together")))
        (should (eq (alist-get 'display-sort-function metadata) 'identity))
        (should (equal (mapcar #'cadr (funcall (alist-get 'affixation-function
                                                          metadata)
                                               texts))
                       '("  " "▶ " "  "))))
      (let ((sent (lambda ()
                    (seq-remove (lambda (words) (equal (cadr words) "status"))
                                (lyrion-test--commands-after-subscribing)))))
        (should (lyrion-test--wait sent))
        (should (equal (funcall sent)
                       '(("02:00:00:00:00:01" "playlist" "index" "2"))))))))

(ert-deftest lyrion-test/a-track-queued-twice-is-told-apart ()
  "Without positions, the second copy of a track gets a number of its own,
and choosing it plays that copy."
  (lyrion-test--with-server
      (lambda (words)
        (if (equal (cdr words) '("status" "0" "100000" "tags:al"))
            (lyrion--encode
             (append words
                     '("playlist_cur_index:0" "playlist_tracks:2"
                       "playlist index:0" "id:501" "title:Für Alina"
                       "artist:Arvo Pärt" "album:Alina"
                       "playlist index:1" "id:501" "title:Für Alina"
                       "artist:Arvo Pärt" "album:Alina")))
          (lyrion-test--answer words)))
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (let ((texts nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _)
                     (setq texts (all-completions "" table))
                     (cadr texts))))
          (lyrion-queue))
        (should (equal (mapcar #'substring-no-properties texts)
                       '("Für Alina  Arvo Pärt · Alina"
                         "Für Alina  Arvo Pärt · Alina (2)"))))
      (should (lyrion-test--wait
               (lambda ()
                 (seq-find (lambda (words) (equal (cadr words) "playlist"))
                           (lyrion-test--commands-after-subscribing)))))
      (should (member '("02:00:00:00:00:01" "playlist" "index" "1")
                      (lyrion-test--commands-after-subscribing))))))

;; Vertico's mode, bound in the tests below without loading Vertico.
(defvar vertico-mode)

(ert-deftest lyrion-test/vertico-starts-on-the-track-playing ()
  "With Vertico, the selection moves to the track playing once it has drawn
the candidates, and only then."
  (let ((moves nil)
        (vertico-mode t))
    (cl-letf (((symbol-function 'vertico--goto)
               (lambda (index) (push (list 'goto index) moves)))
              ((symbol-function 'vertico--exhibit)
               (lambda () (push 'exhibit moves))))
      (with-temp-buffer
        (lyrion--select-on-open 13)
        (should-not moves)
        (run-hooks 'post-command-hook)
        (should (equal (reverse moves) '((goto 13) exhibit)))
        ;; Later commands leave the selection alone.
        (run-hooks 'post-command-hook)
        (should (= 2 (length moves)))))))

(ert-deftest lyrion-test/other-interfaces-start-at-the-top ()
  (let ((vertico-mode nil))
    (with-temp-buffer
      (lyrion--select-on-open 13)
      (should-not (local-variable-p 'post-command-hook)))))

(ert-deftest lyrion-test/choosing-the-track-playing-starts-it-again ()
  "Choosing the track playing from the queue plays it from the start."
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   ;; The fake queue plays its second track.
                   (cadr (all-completions "" table)))))
        (lyrion-queue))
      (let ((sent (lambda ()
                    (seq-remove (lambda (words) (equal (cadr words) "status"))
                                (lyrion-test--commands-after-subscribing)))))
        (should (lyrion-test--wait sent))
        (should (equal (funcall sent)
                       '(("02:00:00:00:00:01" "playlist" "index" "1"))))))))

(ert-deftest lyrion-test/an-empty-queue-is-said-so ()
  (lyrion-test--with-server
      (lambda (words)
        (if (equal (cdr words) '("status" "0" "100000" "tags:al"))
            (lyrion--encode (append words '("playlist_tracks:0")))
          (lyrion-test--answer words)))
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (should-error (lyrion-queue) :type 'user-error))))

(ert-deftest lyrion-test/crossfade-key-in-both-panes ()
  (dolist (map (list lyrion-mode-map lyrion-lyrics-mode-map))
    (should (eq (lookup-key map (kbd "X")) #'lyrion-crossfade))))

(ert-deftest lyrion-test/crossfade-turns-smart-crossfade-on ()
  (should (equal (lyrion-test--crossfade-sends "0")
                 '(("02:00:00:00:00:01" "playerpref" "transitionType" "1")
                   ("02:00:00:00:00:01" "playerpref" "transitionSmart" "1")))))

(ert-deftest lyrion-test/crossfade-turns-any-transition-off ()
  (dolist (type '("1" "4"))
    (should (equal (lyrion-test--crossfade-sends type)
                   '(("02:00:00:00:00:01" "playerpref" "transitionType" "0"))))))

(ert-deftest lyrion-test/crossfade-waits-to-know-the-setting ()
  (lyrion-test--with-server #'ignore
    (lyrion-test--with-player
      (setq lyrion--player '(("playerid" . "02:00:00:00:00:01")))
      (should (lyrion-test--wait (lambda () (process-live-p lyrion--process))))
      (should-error (lyrion-crossfade) :type 'user-error))))

(ert-deftest lyrion-test/connect-asks-for-the-crossfade-setting ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (= 4 (length lyrion-test--setup)))))
      (should (equal lyrion-test--setup
                     '(("subscribe" "prefset")
                       ("02:00:00:00:00:01" "playerpref" "transitionType" "?")
                       ("02:00:00:00:00:01" "playerpref" "transitionSmart" "?")
                       ("02:00:00:00:00:01" "playerpref" "transitionDuration"
                        "?")))))))

(ert-deftest lyrion-test/crossfade-changes-made-elsewhere-are-shown ()
  (let ((lyrion-test--transition-type "1"))
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-player
        (should (lyrion-test--wait
                 (lambda ()
                   (lyrion-test--shows "WiiM Ultra · smart crossfade 10 s"))))
        (dolist (line '("02:37:a6:bc:f5:7a prefset server transitionType 0\n"
                        "02:00:00:00:00:01 prefset server transitionSmart 0\n"
                        "02:00:00:00:00:01 prefset server transitionDuration 5\n"))
          (process-send-string (car lyrion-test--clients) line))
        (should (lyrion-test--wait
                 (lambda () (lyrion-test--shows "WiiM Ultra · crossfade 5 s"))))
        (process-send-string
         (car lyrion-test--clients)
         "02:00:00:00:00:01 prefset server transitionType 0\n")
        (should (lyrion-test--wait
                 (lambda () (not (lyrion-test--shows "crossfade")))))))))

(ert-deftest lyrion-test/render-states-crossfade ()
  (with-temp-buffer
    (setq lyrion--status '(("player_name" . "WiiM Ultra") ("mode" . "play")
                           ("playlist shuffle" . "1") ("title" . "Für Alina")))
    (dolist (case '((("1" "1" "10") . "WiiM Ultra · shuffle · smart crossfade 10 s")
                    (("1" "0" "10") . "WiiM Ultra · shuffle · crossfade 10 s")
                    (("3" "1" "4") . "WiiM Ultra · shuffle · fade 4 s")
                    (("1" "1" "0") . "WiiM Ultra · shuffle · smart crossfade\n")))
      (pcase-let ((`(,type ,smart ,seconds) (car case)))
        (setq lyrion--transition `(("transitionType" . ,type)
                                   ("transitionSmart" . ,smart)
                                   ("transitionDuration" . ,seconds))))
      (lyrion--render)
      (should (string-search (cdr case) (buffer-string))))
    ;; Until its length is known, the crossfade is not shown at all.
    (setq lyrion--transition '(("transitionType" . "1")
                               ("transitionSmart" . "1")))
    (lyrion--render)
    (should-not (string-search "fade" (buffer-string)))
    (setq lyrion--transition '(("transitionType" . "0")))
    (lyrion--render)
    (should-not (string-search "fade" (buffer-string)))))

(ert-deftest lyrion-test/render-states-shuffle-and-repeat ()
  (with-temp-buffer
    (setq lyrion--status '(("player_name" . "WiiM Ultra") ("mode" . "play")
                           ("playlist shuffle" . "1") ("playlist repeat" . "1")
                           ("title" . "Für Alina")))
    (lyrion--render)
    (should (string-search "WiiM Ultra · shuffle · repeat track"
                           (buffer-string))))
  (with-temp-buffer
    (setq lyrion--status '(("player_name" . "WiiM Ultra") ("mode" . "play")
                           ("playlist shuffle" . "2") ("playlist repeat" . "0")
                           ("title" . "Für Alina")))
    (lyrion--render)
    (should (string-search "WiiM Ultra · shuffle" (buffer-string)))
    (should-not (string-search "repeat" (buffer-string))))
  (with-temp-buffer
    (setq lyrion--status '(("player_name" . "WiiM Ultra") ("mode" . "play")
                           ("playlist shuffle" . "0") ("title" . "Für Alina")))
    (lyrion--render)
    (should-not (string-search "shuffle" (buffer-string)))))

;;; This computer as a player

(ert-deftest lyrion-test/local-mac-stays-the-same ()
  "The address of the player Lyrion already knows, with its settings, must not
change: a new one would be a new player."
  (should (equal (lyrion--local-mac "Emacs on example") "02:37:a6:bc:f5:7a"))
  (should (string-match-p "\\`02\\(:[0-9a-f][0-9a-f]\\)\\{5\\}\\'"
                          (lyrion--local-mac "anything"))))

(ert-deftest lyrion-test/squeezelite-command ()
  (let ((lyrion-host "lms.lan")
        (lyrion-squeezelite-name "Emacs on laptop")
        (lyrion-squeezelite-arguments '("-o" "pulse")))
    (should (equal (lyrion--squeezelite-command)
                   (list "squeezelite" "-n" "Emacs on laptop"
                         "-m" (lyrion--local-mac "Emacs on laptop")
                         "-s" "lms.lan" "-C" "5" "-o" "pulse")))
    (let ((lyrion-squeezelite-idle-close nil))
      (should-not (member "-C" (lyrion--squeezelite-command))))))

(defmacro lyrion-test--with-fake-squeezelite (&rest body)
  "Run BODY with squeezelite's start recorded in `started', not run.
The system's own processes are hidden, so a squeezelite running on the
machine that runs the tests does not count as this computer's player."
  (declare (indent 0))
  `(let ((started nil)
         (lyrion--squeezelite nil)
         (real-make-process (symbol-function 'make-process)))
     (cl-letf (((symbol-function 'list-system-processes) #'ignore)
               ((symbol-function 'executable-find) (lambda (_program) "/bin/true"))
               ((symbol-function 'make-process)
                (lambda (&rest arguments)
                  (push (plist-get arguments :command) started)
                  ;; A harmless stand-in that lives as squeezelite would.
                  (funcall real-make-process :name "lyrion-fake-squeezelite"
                           :command '("sleep" "30") :noquery t))))
       (unwind-protect
           (progn ,@body)
         (when (process-live-p lyrion--squeezelite)
           (delete-process lyrion--squeezelite))))))

(ert-deftest lyrion-test/squeezelite-starts-once ()
  (lyrion-test--with-fake-squeezelite
    (should (lyrion--start-squeezelite))
    (should-not (lyrion--start-squeezelite))
    (should (= 1 (length started)))))

(ert-deftest lyrion-test/squeezelite-started-elsewhere-is-used ()
  "A squeezelite with this computer's address, started by another program, is
the player already: a second would fight it for the address."
  (lyrion-test--with-fake-squeezelite
    (let ((lyrion-squeezelite-name "Emacs on example"))
      (cl-letf (((symbol-function 'list-system-processes) (lambda () '(41 42)))
                ((symbol-function 'process-attributes)
                 (lambda (pid)
                   (when (= pid 42)
                     ;; Squeezelite splits its own address argument in place
                     ;; as it reads it, so the system shows spaces, not colons.
                     '((args . "/usr/bin/squeezelite -n Emacs\\ on\\ example -m 02 37 a6 bc f5 7a -s lms.lan -C 5"))))))
        (should-not (lyrion--start-squeezelite))
        (should-not started)))))

(ert-deftest lyrion-test/squeezelite-for-another-address-is-not-ours ()
  (let ((lyrion-squeezelite-name "Emacs on example"))
    (cl-letf (((symbol-function 'list-system-processes) (lambda () '(42)))
              ((symbol-function 'process-attributes)
               (lambda (_pid)
                 '((args . "/usr/bin/squeezelite -n Kitchen -m 02 11 22 33 44 55")))))
      (should-not (lyrion--squeezelite-elsewhere-p)))))

(ert-deftest lyrion-test/squeezelite-missing-is-reported ()
  (let ((lyrion--squeezelite nil))
    (cl-letf (((symbol-function 'executable-find) #'ignore)
              ((symbol-function 'list-system-processes) #'ignore))
      (should-error (lyrion--start-squeezelite) :type 'user-error))))

(ert-deftest lyrion-test/local-player-waits-until-it-connects ()
  "With `local', lyrion starts squeezelite, waits for it to reach the server,
then shows it and sets its volume."
  (let* ((asked 0)
         (local "02:37:a6:bc:f5:7a")
         (answer
          (lambda (words)
            (cond
             ((equal (car words) "players")
              (setq asked (1+ asked))
              ;; It connects on the third look.
              (lyrion--encode
               (list "players" "0" "99" "count:1" "playerindex:0"
                     (concat "playerid:" local) "name:Emacs on example"
                     (if (< asked 3) "connected:0" "connected:1"))))
             ((equal (cadr words) "status")
              (lyrion-test--status-line (car words) "mode:stop"
                                        "title:Für Alina"))))))
    (lyrion-test--with-fake-squeezelite
      (lyrion-test--with-server answer
        (let ((lyrion-player 'local)
              (lyrion-squeezelite-name "Emacs on example")
              (lyrion-local-volume 100)
              (lyrion--local-retry 0.05))
          (unwind-protect
              (progn
                (lyrion)
                (should (= 1 (length started)))
                (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
                (should (= asked 3))
                (should (lyrion-test--wait
                         (lambda () (member (list local "mixer" "volume" "100")
                                            lyrion-test--received))))
                (should (equal (car (seq-find (lambda (words)
                                                (equal (cadr words) "status"))
                                              lyrion-test--received))
                               local)))
            (kill-buffer lyrion-buffer-name)))))))

(ert-deftest lyrion-test/select-player-switches-the-subscription ()
  (lyrion-test--with-server #'lyrion-test--answer
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "WiiM Ultra"))))
      (let ((offered nil))
        (lyrion-test--with-fake-squeezelite
          ;; The fake squeezelite never connects, so after a short wait
          ;; lyrion follows the player anyway, as not connected.
          (let ((lyrion-squeezelite-name "Emacs on example")
                (lyrion--local-wait 0.3)
                (lyrion--local-retry 0.05))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _)
                         (setq offered (mapcar #'car choices))
                         (seq-find (lambda (choice) (string-prefix-p "Emacs" choice))
                                   offered))))
              (lyrion-select-player))
            ;; Choosing this computer's player started squeezelite.
            (should (= 1 (length started)))))
        (should (equal offered '("WiiM Ultra · connected"
                                 "Emacs on example · not connected"))))
      (should (lyrion-test--wait
               (lambda ()
                 (seq-find (lambda (words)
                             (and (equal (car words) "02:37:a6:bc:f5:7a")
                                  (equal (cadr words) "status")))
                           lyrion-test--received)))))))

(ert-deftest lyrion-test/select-player-offers-this-computer-when-unknown ()
  "A server that has never seen this computer's player still offers it."
  (lyrion-test--with-server
      (lambda (words)
        (cond ((equal (car words) "players")
               (lyrion--encode '("players" "0" "99" "count:1" "playerindex:0"
                                 "playerid:02:00:00:00:00:01" "name:WiiM Ultra"
                                 "isplaying:1" "connected:1")))
              ((equal (cadr words) "status")
               (lyrion-test--status-line (car words) "mode:play" "title:Für Alina"))))
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (let ((offered nil)
            (lyrion-squeezelite-name "Emacs on desk"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (setq offered (mapcar #'car choices))
                     (car offered))))
          (lyrion-select-player))
        (should (equal offered '("WiiM Ultra · playing"
                                 "Emacs on desk · this computer")))))))

;;; Lyrics

(defconst lyrion-test--track '("Arvo Pärt" "Für Alina" "Alina" 600.0)
  "A track as `lyrion--track' gives it.")

(defun lyrion-test--rounded (lines)
  "Return LINES with their times rounded to hundredths, for comparing."
  (mapcar (lambda (line)
            (cons (and (car line) (/ (round (* 100 (car line))) 100.0))
                  (cdr line)))
          lines))

(defun lyrion-test--json (status body)
  "Return an HTTP response with STATUS and BODY, a JSON string."
  (format "HTTP/1.0 %d X\r\nContent-Type: application/json\r\n\r\n%s"
          status (encode-coding-string body 'utf-8)))

(defmacro lyrion-test--with-fresh-cache (&rest body)
  "Run BODY with an empty lyrics cache."
  (declare (indent 0))
  `(let ((lyrion--lyrics-cache (make-hash-table :test #'equal)))
     ,@body))

(defun lyrion-test--lookup (track)
  "Look up TRACK's lyrics and return what the callback got."
  (let ((answer 'waiting))
    (lyrion--lyrics-lookup track (lambda (lyrics) (setq answer lyrics)))
    (should (lyrion-test--wait (lambda () (not (eq answer 'waiting))) 3))
    answer))

(ert-deftest lyrion-test/parse-lrc ()
  (should (equal (lyrion-test--rounded
                  (lyrion--parse-lrc
                   (concat "[ar:Al Green]\n"
                           "[00:04.03] Let's stay together\n"
                           "\n"
                           "[00:10.54][01:10.54] I'm so in love\n"
                           "  [00:12.500]\n"
                           "not timed\n")))
                 '((4.03 . "Let's stay together")
                   (10.54 . "I'm so in love")
                   (12.5 . "")
                   (70.54 . "I'm so in love")))))

(ert-deftest lyrion-test/plain-lines ()
  (should (equal (lyrion--plain-lines "\nOne  \n\nTwo\n\n")
                 '((nil . "One") (nil . "") (nil . "Two")))))

(ert-deftest lyrion-test/lyrics-from-entry ()
  (should (equal (lyrion-test--rounded
                  (lyrion--lyrics-from-entry
                   '((syncedLyrics . "[00:01.00] One") (plainLyrics . "Plain"))))
                 '((1.0 . "One"))))
  (should (equal (lyrion--lyrics-from-entry
                  '((syncedLyrics . "  ") (plainLyrics . "Plain")))
                 '((nil . "Plain"))))
  (should (equal (lyrion--lyrics-from-entry '((instrumental . t)))
                 '((nil . "Instrumental"))))
  (should-not (lyrion--lyrics-from-entry '((syncedLyrics) (plainLyrics . ""))))
  (should-not (lyrion--lyrics-from-entry nil)))

(ert-deftest lyrion-test/closest-entry ()
  (let ((entries '(((duration . 200) (plainLyrics . "Far"))
                   ((duration . 231) (plainLyrics . "Near"))
                   ((duration . 229) (plainLyrics . ""))
                   ((duration . 260) (plainLyrics . "Further")))))
    (should (equal (alist-get 'plainLyrics (lyrion--closest-entry entries 229))
                   "Near"))
    (should-not (lyrion--closest-entry entries 400))
    (should (equal (alist-get 'plainLyrics (lyrion--closest-entry entries nil))
                   "Far"))))

(ert-deftest lyrion-test/lyrics-index-leads-a-little ()
  (let ((lines [(4 . "a") (10 . "b") (20 . "c")]))
    (should-not (lyrion--lyrics-index lines 3))
    (should (= (lyrion--lyrics-index lines 3.8) 0))
    (should (= (lyrion--lyrics-index lines 10) 1))
    (should (= (lyrion--lyrics-index lines 100) 2))))

(ert-deftest lyrion-test/lookup-asks-for-the-exact-track ()
  (lyrion-test--with-fresh-cache
    (lyrion-test--with-http
        (lambda (_path)
          (lyrion-test--json
           200 "{\"syncedLyrics\":\"[00:01.00] Eins\\n[00:02.00] Zwei\",\"plainLyrics\":\"x\"}"))
      (should (equal (lyrion-test--rounded (lyrion-test--lookup lyrion-test--track))
                     '((1.0 . "Eins") (2.0 . "Zwei"))))
      (should (equal lyrion-test--requests
                     '("/api/get?artist_name=Arvo%20P%C3%A4rt&track_name=F%C3%BCr%20Alina&album_name=Alina&duration=600")))
      ;; A second look is answered from the cache.
      (lyrion-test--lookup lyrion-test--track)
      (should (= 1 (length lyrion-test--requests))))))

(ert-deftest lyrion-test/lookup-searches-when-there-is-no-exact-match ()
  (lyrion-test--with-fresh-cache
    (lyrion-test--with-http
        (lambda (path)
          (if (string-prefix-p "/api/get" path)
              (lyrion-test--json 404 "{\"message\":\"not found\"}")
            (lyrion-test--json
             200 "[{\"duration\":300,\"plainLyrics\":\"Other\"},{\"duration\":601.5,\"plainLyrics\":\"Für Alina\"}]")))
      (should (equal (lyrion-test--lookup lyrion-test--track)
                     '((nil . "Für Alina"))))
      (should (string-prefix-p "/api/search?artist_name=Arvo%20P%C3%A4rt&track_name="
                               (nth 1 lyrion-test--requests))))))

(ert-deftest lyrion-test/lookup-remembers-that-there-are-none ()
  (lyrion-test--with-fresh-cache
    (lyrion-test--with-http
        (lambda (path)
          (if (string-prefix-p "/api/get" path)
              (lyrion-test--json 404 "{}")
            (lyrion-test--json 200 "[]")))
      (should-not (lyrion-test--lookup lyrion-test--track))
      (should-not (lyrion-test--lookup lyrion-test--track))
      (should (= 2 (length lyrion-test--requests))))))

(ert-deftest lyrion-test/lookup-failure-is-not-remembered ()
  (lyrion-test--with-fresh-cache
    (lyrion-test--with-http (lyrion-test--json 500 "oops")
      (should (eq (lyrion-test--lookup lyrion-test--track) 'failed))
      (should (eq (lyrion-test--lookup lyrion-test--track) 'failed))
      (should (= 2 (length lyrion-test--requests))))))

(ert-deftest lyrion-test/lookup-fails-when-the-search-fails ()
  (lyrion-test--with-fresh-cache
    (lyrion-test--with-http
        (lambda (path)
          (if (string-prefix-p "/api/get" path)
              (lyrion-test--json 404 "{}")
            (lyrion-test--json 500 "oops")))
      (should (eq (lyrion-test--lookup lyrion-test--track) 'failed))
      (should (= 2 (length lyrion-test--requests))))))

(defmacro lyrion-test--in-lyrics (&rest body)
  "Run BODY in a fresh lyrics buffer, killed afterwards."
  (declare (indent 0))
  `(let ((buffer (get-buffer-create lyrion-lyrics-buffer-name)))
     (unwind-protect
         (with-current-buffer buffer
           (lyrion-lyrics-mode)
           ,@body)
       (kill-buffer buffer))))

(ert-deftest lyrion-test/lyrics-render-timed-without-empty-lines ()
  (lyrion-test--in-lyrics
    (lyrion--lyrics-render lyrion-test--track
                           '((1 . "Eins") (2 . "") (3 . "Zwei")))
    (should (string-search "Für Alina\nArvo Pärt\n\nEins\nZwei\n" (buffer-string)))
    (should (= 2 (length lyrion--lyrics-lines)))
    (should (equal (buffer-substring (aref lyrion--lyrics-positions 1)
                                     (+ 4 (aref lyrion--lyrics-positions 1)))
                   "Zwei"))
    (should (eq (get-text-property (aref lyrion--lyrics-positions 0) 'face)
                'lyrion-lyrics))))

(ert-deftest lyrion-test/lyrics-render-can-keep-empty-lines ()
  (lyrion-test--in-lyrics
    (let ((lyrion-lyrics-hide-empty-lines nil))
      (lyrion--lyrics-render lyrion-test--track
                             '((1 . "Eins") (2 . "") (3 . "Zwei")))
      (should (string-search "Eins\n\nZwei\n" (buffer-string)))
      (should (= 3 (length lyrion--lyrics-lines))))))

(ert-deftest lyrion-test/lyrics-render-plain ()
  (lyrion-test--in-lyrics
    (lyrion--lyrics-render lyrion-test--track '((nil . "Eins") (nil . "Zwei")))
    (should (string-search "Eins\nZwei\n" (buffer-string)))
    ;; Untimed lyrics have no line to light, and are not dimmed.
    (should-not lyrion--lyrics-lines)
    (should-not (get-text-property (aref lyrion--lyrics-positions 0) 'face))))

(ert-deftest lyrion-test/lyrics-render-a-notice ()
  (lyrion-test--in-lyrics
    (lyrion--lyrics-render lyrion-test--track nil)
    (should (string-search "No lyrics found" (buffer-string)))
    (lyrion--lyrics-render lyrion-test--track '((1 . "Eins")) "Could not reach LRCLIB")
    (should (string-search "Could not reach LRCLIB" (buffer-string)))
    (should-not (string-search "Eins" (buffer-string)))
    (should-not lyrion--lyrics-lines)))

(ert-deftest lyrion-test/lyrics-load-says-when-nothing-plays ()
  (lyrion-test--in-lyrics
    (setq lyrion--lyrics-track lyrion-test--track)
    (lyrion--lyrics-load (current-buffer) nil)
    (should-not lyrion--lyrics-track)
    (should (string-search "Nothing playing" (buffer-string)))))

(ert-deftest lyrion-test/lyrics-load-says-when-lrclib-fails ()
  (lyrion-test--with-fresh-cache
    (lyrion-test--with-http (lyrion-test--json 500 "oops")
      (lyrion-test--in-lyrics
        (lyrion--lyrics-load (current-buffer) lyrion-test--track)
        (should (string-search "Looking up the lyrics" (buffer-string)))
        (should (lyrion-test--wait
                 (lambda () (string-search "Could not reach LRCLIB"
                                           (buffer-string)))))))))

(ert-deftest lyrion-test/lyrics-text-has-the-words-alone ()
  (should (equal (lyrion--lyrics-text '((1 . "Eins") (2 . "") (3 . "  Zwei ")))
                 "Eins\nZwei\n"))
  (should (equal (lyrion--lyrics-text '((nil . "Eins") (nil . "") (nil . "Zwei")))
                 "Eins\nZwei\n"))
  (should-not (lyrion--lyrics-text '((1 . ""))))
  (should-not (lyrion--lyrics-text nil)))

(ert-deftest lyrion-test/c-copies-the-lyrics-in-both-panes ()
  (should (eq (lookup-key lyrion-mode-map (kbd "C")) #'lyrion-lyrics-copy))
  (should (eq (lookup-key lyrion-lyrics-mode-map (kbd "C")) #'lyrion-lyrics-copy)))

(ert-deftest lyrion-test/lyrics-copy-takes-the-shown-lyrics ()
  (let ((window-buffer (window-buffer (selected-window)))
        (kill-ring nil)
        (interprogram-cut-function nil))
    (lyrion-test--in-lyrics
      ;; Empty lines are copied out, even when they are shown.
      (let ((lyrion-lyrics-hide-empty-lines nil))
        (lyrion--lyrics-render lyrion-test--track
                               '((1 . "Eins") (2 . "") (3 . "Zwei"))))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) (current-buffer))
            (cl-letf (((symbol-function 'message) #'ignore))
              (with-temp-buffer (lyrion-lyrics-copy)))
            (should (equal (car kill-ring) "Eins\nZwei\n")))
        (set-window-buffer (selected-window) window-buffer)))))

(ert-deftest lyrion-test/lyrics-copy-needs-lyrics-showing ()
  (should-error (lyrion-lyrics-copy) :type 'user-error)
  (lyrion-test--in-lyrics
    ;; Loaded but hidden: they may be the previous track's.
    (lyrion--lyrics-render lyrion-test--track '((1 . "Eins")))
    (should-error (lyrion-lyrics-copy) :type 'user-error)
    (let ((window-buffer (window-buffer (selected-window))))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) (current-buffer))
            (lyrion--lyrics-render lyrion-test--track nil)
            (should-error (lyrion-lyrics-copy) :type 'user-error))
        (set-window-buffer (selected-window) window-buffer)))))

(ert-deftest lyrion-test/lyrics-light-the-line-being-sung ()
  (lyrion-test--in-lyrics
    (lyrion--lyrics-render lyrion-test--track '((1 . "Eins") (3 . "Zwei")))
    (lyrion--lyrics-light 1)
    (should (equal (buffer-substring (overlay-start lyrion--lyrics-overlay)
                                     (overlay-end lyrion--lyrics-overlay))
                   "Zwei"))
    (should (eq (overlay-get lyrion--lyrics-overlay 'face) 'lyrion-lyrics-current))))

(ert-deftest lyrion-test/lyrics-tick-follows-the-player ()
  (let ((player (get-buffer-create lyrion-buffer-name))
        (window-buffer (window-buffer (selected-window))))
    (unwind-protect
        (lyrion-test--in-lyrics
          (with-current-buffer player
            ;; 2.8 seconds in, the line at 3 is lit: lines lead a little.
            (setq lyrion--status '(("mode" . "play") ("time" . "2.8"))
                  lyrion--status-time (float-time)))
          (lyrion--lyrics-render lyrion-test--track
                                 '((1 . "Eins") (3 . "Zwei") (9 . "Drei")))
          (set-window-buffer (selected-window) (current-buffer))
          (lyrion--lyrics-tick (current-buffer))
          (should (= lyrion--lyrics-index 1))
          (with-current-buffer player
            (setq lyrion--status '(("mode" . "play") ("time" . "9"))))
          (lyrion--lyrics-tick (current-buffer))
          (should (= lyrion--lyrics-index 2)))
      (set-window-buffer (selected-window) window-buffer)
      (kill-buffer player))))

(ert-deftest lyrion-test/lyrics-follow-new-tracks-only-when-shown ()
  (let ((looked-up nil))
    (cl-letf (((symbol-function 'lyrion--lyrics-lookup)
               (lambda (track callback)
                 (push (nth 1 track) looked-up)
                 (funcall callback (list (cons nil (nth 1 track)))))))
      (lyrion-test--in-lyrics
        (let ((lyrics (current-buffer))
              (window-buffer (window-buffer (selected-window))))
          (with-temp-buffer
            (lyrion--handle-status '(("title" . "Hidden")))
            (should-not looked-up)
            (set-window-buffer (selected-window) lyrics)
            (unwind-protect
                (progn
                  (lyrion--handle-status '(("title" . "One") ("mode" . "play")))
                  (lyrion--handle-status '(("title" . "One") ("mode" . "pause")))
                  (lyrion--handle-status '(("title" . "Two") ("mode" . "play"))))
              (set-window-buffer (selected-window) window-buffer)))
          (should (equal (nreverse looked-up) '("One" "Two")))
          (should (string-search "Two" (with-current-buffer lyrics
                                         (buffer-string)))))))))

(ert-deftest lyrion-test/lyrics-toggle-in-a-side-window ()
  (let ((player (get-buffer-create lyrion-buffer-name)))
    (cl-letf (((symbol-function 'lyrion--lyrics-lookup)
               (lambda (_track callback) (funcall callback '((nil . "Eins"))))))
      (unwind-protect
          (progn
            (with-current-buffer player
              (setq lyrion--status '(("title" . "Für Alina") ("artist" . "Arvo Pärt"))))
            (lyrion-lyrics)
            (let ((window (get-buffer-window lyrion-lyrics-buffer-name)))
              (should (window-live-p window))
              (should (eq (window-parameter window 'window-side) 'right))
              (should (eq (selected-window) window))
              (should (string-search "Eins" (with-current-buffer
                                                lyrion-lyrics-buffer-name
                                              (buffer-string)))))
            (lyrion-lyrics)
            (should-not (get-buffer-window lyrion-lyrics-buffer-name)))
        (when (get-buffer lyrion-lyrics-buffer-name)
          (kill-buffer lyrion-lyrics-buffer-name))
        (kill-buffer player)))))

(ert-deftest lyrion-test/q-in-the-player-hides-the-lyrics-too ()
  "Burying the player alone would leave the lyrics beside an unrelated buffer."
  (should (eq (lookup-key lyrion-mode-map (kbd "q")) #'lyrion-quit))
  (let ((player (get-buffer-create lyrion-buffer-name))
        (other (current-buffer)))
    (cl-letf (((symbol-function 'lyrion--lyrics-lookup)
               (lambda (_track callback) (funcall callback '((nil . "Eins"))))))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer player)
            (let ((lyrion-lyrics-select-window nil))
              (lyrion-lyrics))
            (should (get-buffer-window lyrion-lyrics-buffer-name))
            (with-current-buffer player (lyrion-quit))
            (should-not (get-buffer-window lyrion-lyrics-buffer-name))
            (should-not (get-buffer-window player))
            (should (buffer-live-p player))
            (should (buffer-live-p other)))
        (when (get-buffer lyrion-lyrics-buffer-name)
          (kill-buffer lyrion-lyrics-buffer-name))
        (kill-buffer player)))))

(ert-deftest lyrion-test/closing-the-player-closes-the-lyrics ()
  (lyrion-test--with-server #'lyrion-test--answer
    (let ((lyrics (get-buffer-create lyrion-lyrics-buffer-name)))
      (with-current-buffer lyrics (lyrion-lyrics-mode))
      (lyrion-test--with-player
        (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina")))))
      (should-not (buffer-live-p lyrics)))))

(provide 'lyrion-test)

;;; Qobuz

(defconst lyrion-test--qobuz
  '(("0" "title:Releases"
     "id:0.0_mahler%209.0.0"
     "name:Semyon Bychkov - Mahler: Symphonies 1-9\n(2026)"
     "type:playlist" "isaudio:1"
     "id:0.0_mahler%209.0.1"
     "name:Claudio Abbado - Mahler: Symphony No. 9\n(1999)"
     "type:playlist" "isaudio:1")
    ("1" "title:Artists"
     "id:0.0_mahler%209.1.0" "name:Gustav Mahler" "isaudio:0")
    ("2" "title:Songs"
     "id:0.0_mahler%209.2.0"
     "name:I. Andante comodo by Claudio Abbado from Mahler: Symphony No. 9 "
     "isaudio:1")
    ("3" "title:Playlists")
    ("0.0_mahler%209.1.0.0" "title:Releases"
     "id:0.0_mahler%209.1.0.0.0" "name:Albums (1)" "type:header"
     "id:0.0_mahler%209.1.0.0.1"
     "name:Gustav Mahler - Mahler Plays Mahler\n(1993)"
     "type:playlist" "isaudio:1"))
  "The fake Qobuz: the fields each branch of the search, or an artist's
releases, answers with. Playlists find nothing.")

(defconst lyrion-test--radio-paradise
  '((nil "id:5e.0" "name:Main Mix" "type:outline" "isaudio:1" "hasitems:1"
         "id:5e.5" "name:Serenity - AAC 64kbps" "type:audio" "isaudio:1"
         "hasitems:0")
    ("5e.0" "title:Main Mix"
     "id:5e.0.0" "name:Main Mix - FLAC Interactive" "isaudio:1"
     "id:5e.0.1" "name:Main Mix - AAC 320kbps" "isaudio:1"))
  "The fake Radio Paradise: its channels, and the streams of one.")

(defun lyrion-test--answer-qobuz (words)
  "Answer WORDS as `lyrion-test--answer' would, on a server with Qobuz and
Radio Paradise."
  (cond ((equal (car words) "can")
         (lyrion--encode (append (butlast words) '("1"))))
        ((equal (nthcdr 1 (seq-take words 3)) '("radioparadise" "items"))
         (let ((item-id (seq-find (lambda (word)
                                    (string-prefix-p "item_id:" word))
                                  words)))
           (lyrion--encode
            (append words
                    (cdr (assoc (and item-id (substring item-id 8))
                                lyrion-test--radio-paradise))))))
        ((equal (cadr words) "qobuz")
         (let* ((item-id (substring (seq-find (lambda (word)
                                                (string-prefix-p "item_id:" word))
                                              words)
                                    (length "item_id:")))
                (branch (if (string-prefix-p "0.0." item-id)
                            (substring item-id (length "0.0."))
                          item-id)))
           (lyrion--encode (append words
                                   (cdr (assoc branch lyrion-test--qobuz))
                                   '("count:9")))))
        (t (lyrion-test--answer words))))

(defmacro lyrion-test--with-categories (categories &rest body)
  "Run BODY with CATEGORIES, switch arguments such as \"releases\", saved."
  (declare (indent 1))
  `(let ((transient-values (list (cons 'lyrion-search-menu ,categories)))
         (transient-current-command nil))
     ,@body))

(defun lyrion-test--qobuz-sends (categories choose &optional then key)
  "Search Qobuz for \"mahler 9\" in CATEGORIES, choosing the result CHOOSE
starts with, then THEN among an artist's releases. KEY, `shuffle' or
`open', accepts the first choice as S-RET or M-RET would.
Return the commands sent for the player and the texts offered first."
  (lyrion-test--with-server #'lyrion-test--answer-qobuz
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (let ((offered nil))
        (lyrion-test--with-categories categories
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "mahler 9"))
                    ((symbol-function 'message) #'ignore)
                    ((symbol-function 'completing-read)
                     (lambda (_prompt table &rest _)
                       (let ((texts (all-completions "" table)))
                         (unless offered (setq offered texts))
                         (when (eq texts offered)
                           (pcase key
                             ('shuffle (setq lyrion--shuffle-chosen t))
                             ('open (setq lyrion--open-chosen t))))
                         (seq-find (lambda (text)
                                     (string-prefix-p (if (eq texts offered)
                                                          choose
                                                        then)
                                                      text))
                                   texts)))))
            (lyrion-search-qobuz)))
        (should (lyrion-test--wait
                 (lambda () (seq-find (lambda (words)
                                        (equal (cdr words) '("qobuz" "playlist" "play")))
                                      (mapcar (lambda (words) (seq-take words 4))
                                              lyrion-test--received)))))
        (list (seq-filter (lambda (words) (equal (car words) "02:00:00:00:00:01"))
                          (cddr lyrion-test--received))
              offered)))))

(ert-deftest lyrion-test/a-qobuz-release-name-is-taken-apart ()
  (let ((item (lyrion--qobuz-release
               '(("id" . "0.0_x.0.0")
                 ("name" . "Wiener Philharmoniker - Brahms: Symphony No. 4 - Live\n(1981)")))))
    (should (equal (lyrion--get item "artist") "Wiener Philharmoniker"))
    (should (equal (lyrion--get item "album") "Brahms: Symphony No. 4 - Live"))
    (should (equal (lyrion--get item "year") "1981")))
  ;; As decoded, the year's line is joined with a space.
  (should (equal (lyrion--get (lyrion--qobuz-release '(("name" . "A - T (Live) (2019)")))
                              "album")
                 "T (Live)"))
  (let ((item (lyrion--qobuz-release '(("name" . "Untitled")))))
    (should (equal (lyrion--get item "album") "Untitled"))
    (should-not (lyrion--get item "artist"))))

(ert-deftest lyrion-test/a-qobuz-track-name-is-taken-apart ()
  (let ((item (lyrion--qobuz-track
               '(("name" . "Stand by Me by Ben E. King from Don't Play That Song ")))))
    (should (equal (lyrion--get item "title") "Stand by Me"))
    (should (equal (lyrion--get item "artist") "Ben E. King"))
    (should (equal (lyrion--get item "album") "Don't Play That Song")))
  (let ((item (lyrion--qobuz-track
               '(("name" . "Overture by A from Music from the Film")))))
    (should (equal (lyrion--get item "artist") "A"))
    (should (equal (lyrion--get item "album") "Music from the Film"))))

(ert-deftest lyrion-test/plugins-are-found-once-a-session ()
  (let ((lyrion--plugins-found nil))
    (lyrion-test--with-server #'lyrion-test--answer-qobuz
      (lyrion--check-plugins)
      (should (lyrion--qobuz-p))
      (should (lyrion--radio-paradise-p))
      (lyrion--check-plugins)
      (should (= 2 (cl-count "can" lyrion-test--received
                             :key #'car :test #'equal))))))

(ert-deftest lyrion-test/a-missing-plugin-is-not-offered ()
  (let ((lyrion--plugins-found nil))
    (lyrion-test--with-server
        (lambda (words)
          (when (equal (car words) "can")
            (lyrion--encode (append (butlast words)
                                    (list (if (equal (cadr words) "qobuz")
                                              "1"
                                            "0"))))))
      (lyrion--check-plugins)
      (should (lyrion--qobuz-p))
      (should-not (lyrion--radio-paradise-p))
      (should (assoc "radioparadise" lyrion--plugins-found)))))

(ert-deftest lyrion-test/plugins-are-asked-before-the-library-is-listed ()
  "The server answers one request at a time, so the plugins go first: behind
the library's listings their answers would come too late for the menu."
  (let ((lyrion--plugins-found nil))
    (lyrion-test--with-fresh-library
      (lyrion-test--with-server #'lyrion-test--answer-qobuz
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (lyrion-search))
        (should (lyrion-test--wait
                 (lambda () (= 6 (length (lyrion-test--queries))))))
        (let ((firsts (mapcar #'car lyrion-test--received)))
          (should (equal (seq-take firsts 2) '("can" "can")))
          (should-not (member "can" (nthcdr 2 firsts))))))))

(ert-deftest lyrion-test/plugins-unanswered-are-asked-again ()
  (let ((lyrion--plugins-found nil)
        (lyrion--plugins-timeout 0.2))
    (lyrion-test--with-server #'ignore
      (lyrion--check-plugins)
      (should-not (lyrion--qobuz-p))
      (should-not lyrion--plugins-found))))

(ert-deftest lyrion-test/search-menu-offers-qobuz-only-when-there ()
  (let ((layout (format "%S" (get 'lyrion-search-menu 'transient--layout))))
    (should (string-search "lyrion-search-qobuz" layout))
    (should (string-search ":if lyrion--qobuz-p" layout))))

(ert-deftest lyrion-test/search-menu-has-a-switch-per-category ()
  (let ((suffixes (lyrion-test--suffixes
                   (get 'lyrion-search-menu 'transient--layout))))
    (dolist (key '("-a" "-g" "-p" "-r" "-w" "-t"))
      (should (assoc key suffixes))))
  (let ((layout (format "%S" (get 'lyrion-search-menu 'transient--layout))))
    (should (string-search "C-x C-s saves" layout))
    (dolist (category lyrion--categories)
      (should (string-search (format "%S" (car category)) layout)))))

(ert-deftest lyrion-test/saved-categories-win-over-every-one ()
  (let ((transient-values '((lyrion-search-menu "releases" "works")))
        (menu (clone (get 'lyrion-search-menu 'transient--prefix))))
    (transient-init-value menu)
    (should (equal (oref menu value) '("releases" "works"))))
  (let ((transient-values nil)
        (menu (clone (get 'lyrion-search-menu 'transient--prefix))))
    (transient-init-value menu)
    (should (equal (oref menu value) (mapcar #'car lyrion--categories)))))

(ert-deftest lyrion-test/categories-set-this-session-win-over-saved ()
  "\\`C-x s' sets the menu's value on its prototype, for this session."
  (let ((prototype (get 'lyrion-search-menu 'transient--prefix))
        (transient-values '((lyrion-search-menu "releases")))
        (transient-current-command nil))
    (unwind-protect
        (progn
          (oset prototype value '("works"))
          (should (equal (lyrion--chosen-kinds nil) '(work))))
      (slot-makeunbound prototype 'value))
    (should (equal (lyrion--chosen-kinds nil) '(release)))))

(ert-deftest lyrion-test/chosen-categories-give-kinds-in-order ()
  (lyrion-test--with-categories '("tracks" "artists" "works")
    (should (equal (lyrion--chosen-kinds nil) '(artist work track)))
    (should (equal (lyrion--chosen-kinds t) '(qobuz-artist qobuz-track))))
  (lyrion-test--with-categories '("works" "genres")
    (should (equal (lyrion--chosen-kinds nil) '(genre work)))
    (should-error (lyrion--chosen-kinds t) :type 'user-error))
  ;; Nothing saved: every category.
  (let ((transient-values nil)
        (transient-current-command nil))
    (should (equal (lyrion--chosen-kinds nil)
                   '(artist genre playlist release work track)))))

(ert-deftest lyrion-test/everything-leaves-out-categories-switched-off ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server #'lyrion-test--answer
      (lyrion-test--with-categories '("releases" "playlists")
        (lyrion-test--choosing "Sunday Morning"
          (cl-letf (((symbol-function 'lyrion--play-item) #'ignore))
            (lyrion-search-all)))
        (let ((group (alist-get 'group-function
                                (cdr (funcall lyrion-test--table "" nil 'metadata)))))
          (should (equal (delete-dups
                          (mapcar (lambda (text) (funcall group text nil))
                                  (all-completions "" lyrion-test--table)))
                         '("Playlists" "Releases"))))
        (should (equal (sort (lyrion-test--queries) #'string<)
                       '("albums" "playlists")))))))

(ert-deftest lyrion-test/an-empty-category-is-listed-not-failed ()
  (lyrion-test--with-fresh-library
    (lyrion-test--with-server
        (lambda (words)
          (if (equal (car words) "playlists")
              (lyrion--encode (append words '("count:0")))
            (lyrion-test--answer words)))
      (should (equal (lyrion--library-items '(playlist release))
                     (list (cons 'playlist nil)
                           (cons 'release (alist-get 'release lyrion--library))))))))

(ert-deftest lyrion-test/a-chosen-playlist-plays ()
  (should (equal (lyrion-test--search-sends #'lyrion-search-all "Sunday Morning")
                 '(("playlist" "shuffle" "0")
                   ("playlistcontrol" "cmd:load" "playlist_id:71")))))

(ert-deftest lyrion-test/qobuz-searches-each-chosen-category ()
  (pcase-let ((`(,sent ,offered)
               (lyrion-test--qobuz-sends
                '("releases" "tracks" "playlists" "works")
                "Mahler: Symphony No. 9")))
    ;; Releases, then tracks; playlists found nothing, and works are not
    ;; Qobuz's.
    (should (equal (mapcar #'substring-no-properties offered)
                   (mapcar #'substring-no-properties
                           (append (car (lyrion--candidates-of
                                         'qobuz-release
                                         (lyrion--qobuz-items
                                          'qobuz-release
                                          (cdr (assoc "0" lyrion-test--qobuz)))))
                                   (car (lyrion--candidates-of
                                         'qobuz-track
                                         (lyrion--qobuz-items
                                          'qobuz-track
                                          (cdr (assoc "2" lyrion-test--qobuz)))))))))
    (should (string-search "Semyon Bychkov · 2026" (car offered)))
    (should (equal (sort (mapcar (lambda (words) (nth 5 words))
                                 (seq-filter (lambda (words)
                                               (equal (nth 2 words) "items"))
                                             sent))
                         #'string<)
                   '("item_id:0.0.0" "item_id:0.0.2" "item_id:0.0.3")))
    (should (member '("02:00:00:00:00:01" "qobuz" "playlist" "play"
                      "item_id:0.0_mahler%209.0.1")
                    sent))))

(ert-deftest lyrion-test/a-chosen-qobuz-track-plays ()
  (should (member '("02:00:00:00:00:01" "qobuz" "playlist" "play"
                    "item_id:0.0_mahler%209.2.0")
                  (car (lyrion-test--qobuz-sends '("tracks") "I. Andante")))))

(ert-deftest lyrion-test/a-chosen-qobuz-artist-plays-their-songs ()
  "RET on an artist plays everything by them, as in the library."
  (let ((sent (car (lyrion-test--qobuz-sends '("artists") "Gustav Mahler"))))
    (should (member '("02:00:00:00:00:01" "playlist" "shuffle" "0") sent))
    (should (member '("02:00:00:00:00:01" "qobuz" "playlist" "play"
                      "item_id:0.0_mahler%209.1.0.1")
                    sent))
    ;; Their releases are not asked for.
    (should-not (member '("02:00:00:00:00:01" "qobuz" "items" "0" "100"
                          "item_id:0.0_mahler%209.1.0.0")
                        sent))))

(ert-deftest lyrion-test/a-qobuz-artist-shuffled-plays-their-songs ()
  (let ((sent (car (lyrion-test--qobuz-sends '("artists") "Gustav Mahler"
                                             nil 'shuffle))))
    (should (member '("02:00:00:00:00:01" "playlist" "shuffle" "1") sent))
    (should (member '("02:00:00:00:00:01" "qobuz" "playlist" "play"
                      "item_id:0.0_mahler%209.1.0.1")
                    sent))))

(ert-deftest lyrion-test/an-opened-qobuz-artist-offers-their-releases ()
  "M-RET on an artist opens their releases, as in the library."
  (let ((sent (car (lyrion-test--qobuz-sends '("artists") "Gustav Mahler"
                                             "Mahler Plays Mahler" 'open))))
    (should (member '("02:00:00:00:00:01" "qobuz" "items" "0" "100"
                      "item_id:0.0_mahler%209.1.0.0")
                    sent))
    (should (member '("02:00:00:00:00:01" "playlist" "shuffle" "0") sent))
    (should (member '("02:00:00:00:00:01" "qobuz" "playlist" "play"
                      "item_id:0.0_mahler%209.1.0.0.1")
                    sent))))

(defun lyrion-test--radio-paradise-plays (channel)
  "Choose CHANNEL from Radio Paradise, and return the command it played."
  (lyrion-test--with-server #'lyrion-test--answer-qobuz
    (lyrion-test--with-player
      (should (lyrion-test--wait (lambda () (lyrion-test--shows "Für Alina"))))
      (let ((offered nil))
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _)
                     (setq offered (funcall (alist-get 'display-sort-function
                                                       (cdr (funcall table "" nil
                                                                     'metadata)))
                                            (all-completions "" table)))
                     channel)))
          (lyrion-radio-paradise))
        (should (equal offered '("Main Mix" "Serenity - AAC 64kbps"))))
      (should (lyrion-test--wait
               (lambda () (seq-find (lambda (words)
                                      (equal (cadr words) "radioparadise"))
                                    (seq-filter (lambda (words)
                                                  (equal (nth 2 words) "playlist"))
                                                lyrion-test--received)))))
      (seq-find (lambda (words) (equal (nth 2 words) "playlist"))
                lyrion-test--received))))

(ert-deftest lyrion-test/a-radio-paradise-channel-plays-its-first-stream ()
  (should (equal (lyrion-test--radio-paradise-plays "Main Mix")
                 '("02:00:00:00:00:01" "radioparadise" "playlist" "play"
                   "item_id:5e.0.0"))))

(ert-deftest lyrion-test/a-radio-paradise-stream-plays-as-it-is ()
  (should (equal (lyrion-test--radio-paradise-plays "Serenity - AAC 64kbps")
                 '("02:00:00:00:00:01" "radioparadise" "playlist" "play"
                   "item_id:5e.5"))))

(ert-deftest lyrion-test/search-menu-offers-radio-paradise-only-when-there ()
  (let ((layout (format "%S" (get 'lyrion-search-menu 'transient--layout))))
    (should (string-search "lyrion-radio-paradise" layout))
    (should (string-search ":if lyrion--radio-paradise-p" layout))))

;;; lyrion-test.el ends here
