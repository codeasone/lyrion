;;; lyrion.el --- A minimal front end for Lyrion Music Server -*- lexical-binding: t -*-

;; SPDX-License-Identifier: CC0-1.0
;;
;; To the extent possible under law, Mark Stuart has waived all copyright
;; and related or neighbouring rights to lyrion, if any exist: its code was
;; written by an AI model. This work is published from: United Kingdom.
;; See the LICENSE file for the full text of CC0 1.0 Universal.

;; Maintainer: Mark Stuart <codeasone@gmail.com>
;; Assisted-by: Claude:claude-opus-5-5
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: multimedia
;; URL: https://github.com/codeasone/lyrion

;;; Commentary:

;; `lyrion' is a minimal front end for a Lyrion Music Server (formerly
;; Logitech Media Server). It shows what a player is playing: the cover,
;; the track and how far through it is, the track up next, and the track's
;; lyrics from LRCLIB beside it. It searches the library for tracks,
;; releases, album artists, genres, works and playlists. RET plays the
;; choice in order, S-RET shuffled, and M-RET opens an artist into their
;; releases, to pick one. When the server has the plugins, it also searches
;; and streams Qobuz, with the same keys, and plays Radio Paradise's
;; channels.
;;
;; Single keys play and pause, move between and within tracks, change the
;; volume, shuffle and repeat, turn smart crossfading on or off, set a sleep
;; timer, and switch between the server's players, one of which can be this
;; computer, through squeezelite.
;;
;; The server owns the queue; `lyrion' keeps no copy of it, and shows it
;; only when asked, fetching it afresh, to jump to a track in it.
;;
;; It keeps one connection to the server's command-line interface, on port
;; 9090, and subscribes to the player's status. The server then sends the
;; whole status again whenever anything changes, so nothing is polled; the
;; elapsed time is counted here between changes. Search lists the library
;; on connections of its own, so the player is never held up. Covers come
;; from the server's web port and lyrics from LRCLIB over TLS, through a
;; small HTTP client of its own rather than `url.el'.
;;
;; Developed with Claude Opus 5.5 (Anthropic) in Claude Code.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'text-property-search)
(require 'transient)
(require 'url-parse)
(require 'url-util)

(defgroup lyrion nil
  "A minimal front end for Lyrion Music Server."
  :group 'multimedia
  :prefix "lyrion-")

(defcustom lyrion-host "localhost"
  "Host name or address of the Lyrion Music Server."
  :type 'string)

(defcustom lyrion-cli-port 9090
  "Port of the server's command-line interface."
  :type 'natnum)

(defcustom lyrion-http-port 9000
  "Port of the server's web interface, which serves the covers."
  :type 'natnum)

(defcustom lyrion-player nil
  "Player to show: a name or id, `local', or nil for the one that is playing.
A name is compared without regard to case. `local' starts squeezelite so
this computer is a player, and shows that. With nil, the first player that
is connected and playing is chosen, then the first that is connected.
\\<lyrion-mode-map>\\[lyrion-select-player] chooses another for the session."
  :type '(choice (const :tag "The one playing" nil)
                 (const :tag "This computer, through squeezelite" local)
                 (string :tag "A player's name or id")))

(defcustom lyrion-squeezelite-program "squeezelite"
  "Program that makes this computer a player."
  :type 'string)

(defcustom lyrion-squeezelite-name (format "Emacs on %s" (system-name))
  "Name this computer's player has in Lyrion.
Its address is derived from the name, so Lyrion keeps the player's settings,
such as its volume and ReplayGain, from one session to the next."
  :type 'string)

(defcustom lyrion-squeezelite-arguments nil
  "Further arguments for squeezelite, such as (\"-o\" \"pipewire\").
squeezelite -l lists the output devices. Prefer a shared one: a `hw:'
device is held exclusively, so nothing else on this computer can play."
  :type '(repeat string))

(defcustom lyrion-squeezelite-idle-close 5
  "Seconds of silence after which squeezelite lets go of the sound device.
Nil keeps it, which saves the moment it takes to open it again."
  :type '(choice (const :tag "Keep the device" nil) natnum))

(defcustom lyrion-local-volume nil
  "Volume, 0 to 100, given to this computer's player when lyrion starts it.
Nil leaves the volume Lyrion has for it."
  :type '(choice (const :tag "As Lyrion has it" nil) natnum))

(defcustom lyrion-skip-seconds 10
  "Seconds the arrow keys skip back or forward in the track."
  :type 'number)

(defcustom lyrion-volume-step 5
  "How much each press of a volume key changes the volume, out of 100."
  :type 'natnum)

(defcustom lyrion-cover-size 32
  "Longest side of the cover, in em: multiples of the default font's size."
  :type 'number)

(defcustom lyrion-progress-width 32
  "Width of the progress bar, in columns."
  :type 'natnum)

(defcustom lyrion-reconnect-delay 5
  "Seconds to wait before connecting again after the connection drops."
  :type 'number)

(defcustom lyrion-lyrics-url "https://lrclib.net/api"
  "Base address of the LRCLIB API, where the lyrics come from.
Showing the lyrics sends the track's artist, title, album and length there."
  :type 'string)

(defcustom lyrion-lyrics-side 'right
  "Side of the frame the lyrics open on."
  :type '(choice (const right) (const left) (const bottom) (const top)))

(defcustom lyrion-lyrics-size 0.33
  "Share of the frame the lyrics take: their width at the sides."
  :type 'number)

(defcustom lyrion-lyrics-select-window t
  "Whether showing the lyrics moves point into them."
  :type 'boolean)

(defcustom lyrion-lyrics-hide-empty-lines t
  "Whether the lyrics leave out lines with no words.
Timed lyrics mark the end of a line with an empty one; hidden, the line
just sung stays lit until the next begins."
  :type 'boolean)

(defcustom lyrion-qobuz-results 100
  "Most results a Qobuz search shows of each kind."
  :type 'natnum)

(defface lyrion-title '((t :weight bold :height 1.4))
  "Face of the track's title.")

(defface lyrion-track-number '((t :inherit (shadow lyrion-title) :weight normal))
  "Face of the track's number on its release, before the title.")

(defface lyrion-next '((t :inherit default))
  "Face of the title of the track up next.")

(defface lyrion-next-key '((t :inherit font-lock-keyword-face))
  "Face of the key that skips to the track up next.")

(defface lyrion-artist '((t :height 1.1))
  "Face of the track's artist.")

(defface lyrion-album '((t :inherit shadow))
  "Face of the album and the year.")

(defface lyrion-elapsed '((t :inherit font-lock-keyword-face))
  "Face of the played part of the progress bar.")

(defface lyrion-remaining '((t :inherit shadow))
  "Face of the part of the progress bar still to play.")

(defface lyrion-time '((t :inherit shadow))
  "Face of the elapsed and total times.")

(defface lyrion-player '((t :inherit shadow))
  "Face of the player's name and state.")

(defface lyrion-lyrics-title '((t :weight bold :height 1.2))
  "Face of the track's title above its lyrics.")

(defface lyrion-lyrics '((t :inherit shadow))
  "Face of a timed lyrics line that is not being sung.")

(defface lyrion-lyrics-current '((t :inherit default :weight bold))
  "Face of the lyrics line being sung.")

(defface lyrion-search-detail '((t :inherit shadow))
  "Face of the details beside a search result, such as its artist.")

(defface lyrion-icon-track '((t :inherit font-lock-keyword-face))
  "Face of the icon of a track among search results.")

(defface lyrion-icon-release '((t :inherit font-lock-type-face))
  "Face of the icon of a release among search results.")

(defface lyrion-icon-artist '((t :inherit font-lock-function-name-face))
  "Face of the icon of an album artist among search results.")

(defface lyrion-icon-genre '((t :inherit font-lock-builtin-face))
  "Face of the icon of a genre among search results.")

(defface lyrion-icon-work '((t :inherit font-lock-constant-face))
  "Face of the icon of a work among search results.")

(defface lyrion-icon-playlist '((t :inherit font-lock-variable-name-face))
  "Face of the icon of a saved playlist among search results.")

(defface lyrion-icon-qobuz '((t :inherit font-lock-string-face))
  "Face of the icon of anything from Qobuz among search results.")

(defconst lyrion-buffer-name "*lyrion*"
  "Name of the buffer that shows the player.")

(defconst lyrion--transition-prefs
  '("transitionType" "transitionSmart" "transitionDuration")
  "The player preferences that make up its crossfade setting.")

(defconst lyrion--status-tags "tags:alcdyKtiq"
  "The track's fields asked for in its status.
They are the artist, album, cover, duration, year, artwork URL, track
number, disc and number of discs.")

(defconst lyrion--cover-pixels 1600
  "Side of the square the server scales a cover to before sending it.")

(defvar lyrion--http-timeout 10
  "Seconds to wait for a web server to answer, for a cover or lyrics.")

(defconst lyrion--user-agent
  ;; The version is the one in this file's header.
  (format "lyrion/%s (https://github.com/codeasone/lyrion)"
          (package-get-version))
  "How lyrion names itself to web servers; LRCLIB asks clients to say.")

;;; State, local to the player's buffer

(defvar-local lyrion--process nil
  "Connection to the server's command-line interface.")

(defvar-local lyrion--player nil
  "The player shown, as an alist of the fields `players' gives.")

(defvar-local lyrion--status nil
  "The player's latest status, as an alist.")

(defvar-local lyrion--status-time nil
  "When `lyrion--status' arrived, as a float time.")

(defvar-local lyrion--transition nil
  "The player's crossfade settings, as an alist of `lyrion--transition-prefs'.
The status leaves them out, so they are asked for and followed apart.")

(defvar-local lyrion--cover nil
  "The cover for the current track, as (PATH . IMAGE), or nil.
While the cover is on its way, IMAGE is the last track's, if any. It is nil
when the cover cannot be fetched or shown.")

(defvar-local lyrion--notice nil
  "A line shown in place of the track, such as while connecting, or nil.")

(defvar-local lyrion--timer nil
  "Timer that advances the progress line.")

(defvar-local lyrion--retry nil
  "Timer that connects again after the connection dropped.")

(defvar-local lyrion--chosen nil
  "Id of the player chosen with `lyrion-select-player', or nil.
It takes the place of `lyrion-player' for as long as the buffer lives.")

(defvar-local lyrion--local-until nil
  "Time until which to wait for this computer's player to connect, or nil.")

(defvar-local lyrion--local-volume-pending nil
  "Non-nil when this computer's player was just started and needs its volume.")

;;; This computer as a player

(defvar lyrion--squeezelite nil
  "The squeezelite process lyrion started, or nil.
It outlives the player's buffer, so closing lyrion does not stop the music.")

(defvar lyrion--local-wait 10
  "Seconds to wait for a squeezelite just started to connect to the server.")

(defvar lyrion--local-retry 0.5
  "Seconds between looks at whether squeezelite has connected yet.")

(defun lyrion--local-mac (name)
  "Return the address of this computer's player, called NAME.
It is locally administered and derived from NAME, so it is the same from
one session to the next. Changing the derivation would make Lyrion see a new
player, without the settings it keeps for this one."
  (let ((hash (secure-hash 'sha1 name)))
    (concat "02" (mapconcat (lambda (index)
                              (concat ":" (substring hash (* 2 index)
                                                     (* 2 (1+ index)))))
                            (number-sequence 0 4) ""))))

(defun lyrion--squeezelite-command ()
  "Return the command line to start this computer's player."
  (append (list lyrion-squeezelite-program
                "-n" lyrion-squeezelite-name
                "-m" (lyrion--local-mac lyrion-squeezelite-name)
                "-s" lyrion-host)
          (when lyrion-squeezelite-idle-close
            (list "-C" (number-to-string lyrion-squeezelite-idle-close)))
          lyrion-squeezelite-arguments))

(defun lyrion--squeezelite-elsewhere-p ()
  "Return non-nil when a squeezelite with this computer's address is running.
It may be one started by another program or a shell; a second would fight it
for the address."
  ;; Squeezelite splits its address argument in place as it reads it, so the
  ;; system may show the octets apart rather than joined by colons.
  (let ((address (mapconcat #'identity
                            (split-string (lyrion--local-mac
                                           lyrion-squeezelite-name)
                                          ":")
                            "[: ]")))
    (seq-some (lambda (pid)
                (let ((arguments (alist-get 'args (process-attributes pid))))
                  (and arguments
                       (string-search "squeezelite" arguments)
                       (string-match-p address arguments))))
              (list-system-processes))))

(defun lyrion--start-squeezelite ()
  "Start squeezelite unless it is running; return non-nil when started now."
  (unless (or (process-live-p lyrion--squeezelite)
              (lyrion--squeezelite-elsewhere-p))
    (unless (executable-find lyrion-squeezelite-program)
      (user-error "Cannot find %s, which makes this computer a player"
                  lyrion-squeezelite-program))
    (setq lyrion--squeezelite
          (make-process :name "lyrion-squeezelite"
                        :buffer "*lyrion squeezelite*"
                        :command (lyrion--squeezelite-command)
                        :noquery t))
    t))

(defun lyrion--start-local (buffer)
  "Start this computer's player for BUFFER, which waits for it to connect."
  (let ((started (lyrion--start-squeezelite)))
    (with-current-buffer buffer
      (setq lyrion--local-until (+ (float-time) lyrion--local-wait)
            lyrion--local-volume-pending (and started t)))))

(defun lyrion--wanted ()
  "Return the player to show: an id or a name, or nil for the one playing."
  (let ((wanted (or lyrion--chosen lyrion-player)))
    (if (eq wanted 'local)
        (lyrion--local-mac lyrion-squeezelite-name)
      wanted)))

;;; The server's command-line protocol

(defun lyrion--encode (words)
  "Return WORDS as one command line for the server."
  (concat (mapconcat #'url-hexify-string words " ") "\n"))

(defun lyrion--decode (line)
  "Return the words of LINE, a line from the server, decoded."
  (mapcar (lambda (word)
            (decode-coding-string (url-unhex-string word) 'utf-8))
          (split-string line " " t)))

(defun lyrion--fields (words)
  "Return the KEY:VALUE words among WORDS as an alist, in order.
Only the first colon separates: values such as player ids contain more."
  (delq nil (mapcar (lambda (word)
                      (when-let* ((colon (string-search ":" word)))
                        (cons (substring word 0 colon)
                              (substring word (1+ colon)))))
                    words)))

(defun lyrion--get (alist key)
  "Return the value of KEY, a string, in ALIST."
  (cdr (assoc key alist)))

(defun lyrion--number (value)
  "Return VALUE, a string from the server, as a number, or nil."
  (and value
       (string-match-p "\\`-?[0-9]+\\(?:\\.[0-9]+\\)?\\'" value)
       (string-to-number value)))

(defun lyrion--group (words key)
  "Return the items listed in WORDS, each an alist starting at a KEY field.
Fields before the first KEY, such as the echoed command, are left out."
  (let (items)
    (dolist (field (lyrion--fields words))
      (cond ((equal (car field) key) (push (list field) items))
            (items (push field (car items)))))
    (nreverse (mapcar #'reverse items))))

(defun lyrion--parse-players (words)
  "Return the players in WORDS, the reply to `players', each an alist."
  (lyrion--group words "playerindex"))

(defun lyrion--parse-status (words)
  "Return the fields of WORDS, a player's status line, as an alist."
  ;; The first word is the player's id, whose colons are not a field.
  (lyrion--fields (nthcdr 2 words)))

(defun lyrion--next-track (status)
  "Return the track after the one playing in STATUS, as an alist, or nil.
The status lists the tracks from the one playing, each starting at its
`playlist index'. Repeating the track, nothing comes next but itself."
  (unless (equal (lyrion--get status "playlist repeat") "1")
    (let ((tracks nil))
      (dolist (field status)
        (cond ((equal (car field) "playlist index") (push (list field) tracks))
              (tracks (push field (car tracks)))))
      (when (cdr tracks)
        (reverse (nth (- (length tracks) 2) tracks))))))

(defun lyrion--choose-player (players wanted)
  "Return the player among PLAYERS to show, or nil.
WANTED is a player's name or id, or nil for the one playing."
  (let ((on (lambda (player key) (equal (lyrion--get player key) "1"))))
    (if wanted
        (seq-find (lambda (player)
                    (or (equal (lyrion--get player "playerid") wanted)
                        (string-equal-ignore-case
                         (or (lyrion--get player "name") "") wanted)))
                  players)
      ;; A player that lost its connection can still claim to be playing.
      (or (seq-find (lambda (player)
                      (and (funcall on player "isplaying")
                           (funcall on player "connected")))
                    players)
          (seq-find (lambda (player) (funcall on player "connected")) players)
          (car players)))))

;;; Time

(defun lyrion--elapsed (status received now)
  "Return the seconds played of STATUS, received at RECEIVED, as of NOW."
  (let ((time (or (lyrion--number (lyrion--get status "time")) 0))
        (duration (lyrion--number (lyrion--get status "duration"))))
    (when (and received (equal (lyrion--get status "mode") "play"))
      (setq time (+ time (- now received))))
    (if (and duration (> duration 0)) (min time duration) time)))

(defun lyrion--format-time (seconds)
  "Return SECONDS as M:SS, or H:MM:SS from an hour on."
  (let* ((total (max 0 (floor seconds)))
         (hours (/ total 3600))
         (minutes (/ (% total 3600) 60))
         (rest (% total 60)))
    (if (> hours 0)
        (format "%d:%02d:%02d" hours minutes rest)
      (format "%d:%02d" minutes rest))))

;;; Covers

(defun lyrion--cover-path (status)
  "Return the server path of the cover for STATUS's track, or nil."
  (let* ((coverid (lyrion--get status "coverid"))
         (artwork (lyrion--get status "artwork_url"))
         (served-artwork (and artwork (string-prefix-p "/" artwork))))
    (cond
     ;; A streamed track's cover id is a stand-in, which gets a placeholder;
     ;; its cover comes through the server's image proxy instead.
     ((and coverid (not (and served-artwork
                             (equal (lyrion--get status "remote") "1"))))
      (format "/music/%s/cover_%dx%d_o.jpg" (url-hexify-string coverid)
              lyrion--cover-pixels lyrion--cover-pixels))
     (served-artwork
      ;; The proxy resizes, as the library's covers do.
      (if (string-match "/image\\(\\.[a-z]+\\)\\'" artwork)
          (replace-match (format "/image_%dx%d_o\\1" lyrion--cover-pixels
                                 lyrion--cover-pixels)
                         t nil artwork)
        artwork)))))

(defun lyrion--http-response (response)
  "Return RESPONSE, a whole HTTP response, as (STATUS . BODY), or nil."
  (when (string-match "\\`HTTP/[0-9.]+ \\([0-9]+\\)" response)
    (let ((status (string-to-number (match-string 1 response)))
          (end (string-search "\r\n\r\n" response)))
      (cons status (if end (substring response (+ end 4)) "")))))

(defun lyrion--http-get (host port path callback &optional tls)
  "Fetch PATH from HOST on PORT, then call CALLBACK with (STATUS . BODY).
TLS non-nil makes the request over TLS. CALLBACK receives nil when the
request fails or takes longer than `lyrion--http-timeout' seconds. The
request is HTTP/1.0, so the server closes the connection when it is done."
  (let* ((chunks nil)
         (done nil)
         (timer nil)
         (finish (lambda (response)
                   (unless done
                     (setq done t)
                     (when timer (cancel-timer timer))
                     (funcall callback response)))))
    (condition-case nil
        (let ((process (open-network-stream
                        "lyrion-http" nil host port
                        :type (if tls 'tls 'plain) :nowait t :coding 'binary)))
          (set-process-query-on-exit-flag process nil)
          (set-process-filter process (lambda (_process chunk)
                                        (push chunk chunks)))
          (set-process-sentinel
           process
           (lambda (process event)
             (cond
              ((string-prefix-p "open" event)
               (process-send-string
                process
                (format (concat "GET %s HTTP/1.0\r\nHost: %s\r\n"
                                "User-Agent: %s\r\n\r\n")
                        path host lyrion--user-agent)))
              ((not (process-live-p process))
               (funcall finish (lyrion--http-response
                                (apply #'concat (nreverse chunks))))))))
          (setq timer (run-at-time lyrion--http-timeout nil
                                   (lambda ()
                                     (funcall finish nil)
                                     (delete-process process)))))
      (error (funcall finish nil)))))

(defun lyrion--image (data)
  "Return DATA, an image file's contents, as an image to show, or nil."
  (when (display-images-p)
    (ignore-errors
      (create-image data nil t
                    :max-width (cons lyrion-cover-size 'em)
                    :max-height (cons lyrion-cover-size 'em)))))

(defun lyrion--fetch-cover (path buffer)
  "Fetch the cover at PATH for BUFFER, and show it when it arrives.
Failing that, the last track's cover, shown meanwhile, goes."
  (lyrion--http-get
   lyrion-host lyrion-http-port path
   (lambda (response)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         ;; The track may have changed while the cover was on its way.
         (when (equal (car lyrion--cover) path)
           (setcdr lyrion--cover (and (eql (car response) 200)
                                      (lyrion--image (cdr response))))
           (lyrion--render)))))))

;;; Drawing

(defun lyrion--centre (string)
  "Return STRING as a line centred in the window.
The centre is the window's own, so the line stays centred as it resizes."
  (concat (propertize " " 'display
                      `(space :align-to
                              (- center (,(/ (string-pixel-width string) 2)))))
          string "\n"))

(defun lyrion--progress-bar (elapsed duration width)
  "Return a bar WIDTH columns wide, filled for ELAPSED of DURATION seconds."
  (let ((filled (if (and duration (> duration 0))
                    (min width (round (* width (/ elapsed (float duration)))))
                  0)))
    (concat (propertize (make-string filled ?━) 'face 'lyrion-elapsed)
            (propertize (make-string (- width filled) ?─)
                        'face 'lyrion-remaining))))

(defun lyrion--progress-text ()
  "Return the progress line: elapsed time, bar and length."
  (let* ((status lyrion--status)
         (duration (lyrion--number (lyrion--get status "duration")))
         (elapsed (lyrion--elapsed status lyrion--status-time (float-time))))
    (if (and duration (> duration 0))
        (let ((total (lyrion--format-time duration)))
          (concat (propertize (string-pad (lyrion--format-time elapsed)
                                          (length total) nil t)
                              'face 'lyrion-time)
                  "  " (lyrion--progress-bar elapsed duration
                                             lyrion-progress-width)
                  "  " (propertize total 'face 'lyrion-time)))
      (propertize (lyrion--format-time elapsed) 'face 'lyrion-time))))

(defun lyrion--progress-line ()
  "Return the centred progress line, marked so a tick can replace it."
  (propertize (lyrion--centre (lyrion--progress-text)) 'lyrion-progress t))

(defun lyrion--transition-name (transition)
  "Return the name of TRANSITION, a crossfade setting, or nil when unknown.
A crossfade is named once both of its settings are known, to avoid a
flicker as they arrive."
  (pcase (lyrion--get transition "transitionType")
    ("0" "off")
    ("1" (pcase (lyrion--get transition "transitionSmart")
           ("1" "smart crossfade")
           ("0" "crossfade")))
    ((or "2" "3" "4") "fade")))

(defun lyrion--sleep-time (status time)
  "Return when the player in STATUS, received at TIME, will sleep, or nil.
It is a clock time such as 23:45, which needs no counting down."
  (when-let* ((left (lyrion--number (lyrion--get status "will_sleep_in"))))
    (format-time-string "%H:%M" (+ (or time (float-time)) left))))

(defun lyrion--player-text ()
  "Return the player's name, and its state when it is not playing."
  (let ((status lyrion--status))
    (propertize
     (concat (or (lyrion--get status "player_name")
                 (lyrion--get lyrion--player "name")
                 "")
             (pcase (lyrion--get status "mode")
               ("play" "")
               ("pause" " · paused")
               (_ " · stopped"))
             ;; Shuffling by album, set elsewhere, is shown as shuffle too.
             (if (member (lyrion--get status "playlist shuffle") '("1" "2"))
                 " · shuffle"
               "")
             (let ((name (lyrion--transition-name lyrion--transition))
                   (seconds (lyrion--get lyrion--transition
                                         "transitionDuration")))
               ;; Shown once its length is known too, to avoid a flicker.
               (if (or (member name '(nil "off")) (not seconds))
                   ""
                 (concat " · " name
                         (if (equal seconds "0") "" (format " %s s" seconds)))))
             (pcase (lyrion--get status "playlist repeat")
               ("1" " · repeat track")
               ("2" " · repeat all")
               (_ ""))
             (if-let* ((time (lyrion--sleep-time status lyrion--status-time)))
                 (concat " · sleeps at " time)
               "")
             (if (equal (lyrion--get status "player_connected") "0")
                 " · not connected"
               ""))
     'face 'lyrion-player)))

(defun lyrion--track-number (status)
  "Return the number of the track in STATUS on its release, or nil.
On a release of several discs, the disc comes first, as in 2-7."
  (let ((number (lyrion--get status "tracknum"))
        (disc (lyrion--get status "disc")))
    (when (and number (not (equal number "0")))
      (if (and disc (> (or (lyrion--number (lyrion--get status "disccount")) 1)
                       1))
          (format "%s-%s" disc number)
        number))))

(defun lyrion--next-text (status)
  "Return the line naming the track after the one in STATUS, or nil.
Its artist is named only when it is not the one playing. The line ends
with the key that skips to it."
  (when-let* ((next (lyrion--next-track status))
              (title (lyrion--get next "title")))
    (let ((artist (lyrion--get next "artist")))
      (concat (propertize "Up next" 'face 'lyrion-album)
              "  " (propertize title 'face 'lyrion-next)
              (if (and artist (not (equal artist (lyrion--get status "artist"))))
                  (propertize (concat " · " artist) 'face 'lyrion-album)
                "")
              "  " (propertize (substitute-command-keys
                                "\\<lyrion-mode-map>\\[lyrion-next]")
                               'face 'lyrion-next-key)))))

(defun lyrion--render ()
  "Draw the player's buffer from its state."
  (let ((inhibit-read-only t)
        (status lyrion--status)
        (title (or (lyrion--get lyrion--status "title")
                   (lyrion--get lyrion--status "current_title"))))
    (erase-buffer)
    (insert "\n")
    (cond
     (lyrion--notice
      (insert (lyrion--centre (propertize lyrion--notice 'face 'lyrion-album))))
     ((not title)
      (insert (lyrion--centre (propertize "Nothing playing"
                                          'face 'lyrion-album))
              "\n" (lyrion--centre (lyrion--player-text))))
     (t
      (when-let* ((image (cdr lyrion--cover)))
        (insert (lyrion--centre (propertize " " 'display image)) "\n"))
      (insert (lyrion--centre
               (concat (when-let* ((number (lyrion--track-number status)))
                         (propertize (concat number ". ")
                                     'face 'lyrion-track-number))
                       (propertize title 'face 'lyrion-title))))
      (when-let* ((artist (lyrion--get status "artist")))
        (insert (lyrion--centre (propertize artist 'face 'lyrion-artist))))
      (let* ((album (lyrion--get status "album"))
             (year (lyrion--get status "year"))
             (line (string-join
                    (delq nil (list album
                                    (and year (not (equal year "0")) year)))
                    " · ")))
        (unless (string-empty-p line)
          (insert (lyrion--centre (propertize line 'face 'lyrion-album)))))
      (insert "\n" (lyrion--progress-line)
              "\n" (lyrion--centre (lyrion--player-text)))
      ;; Up next, a line apart from the player's state.
      (when-let* ((next (lyrion--next-text status)))
        (insert "\n" (lyrion--centre next)))))
    (goto-char (point-min))))

(defun lyrion--update-progress ()
  "Redraw the progress line alone, leaving the rest of the buffer be."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward 'lyrion-progress t t)))
        (delete-region (prop-match-beginning match) (prop-match-end match))
        (goto-char (prop-match-beginning match))
        (insert (lyrion--progress-line))))))

(defun lyrion--tick (buffer)
  "Advance BUFFER's progress line, while it is shown and the track plays."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (get-buffer-window buffer t)
                 (equal (lyrion--get lyrion--status "mode") "play"))
        (lyrion--update-progress)))))

;;; Lyrics, from LRCLIB

(defconst lyrion-lyrics-buffer-name "*lyrion lyrics*"
  "Name of the buffer that shows the lyrics.")

(defconst lyrion--lrc-time "\\[\\([0-9]+\\):\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\]"
  "A time stamp in timed lyrics, such as [01:23.45].")

(defconst lyrion--lyrics-lead 0.3
  "Seconds before its time that a timed line is lit, as singers lead a little.")

(defconst lyrion--lyrics-tolerance 10
  "Most seconds a search result's length may differ from the track's.
One further off is more likely another recording than the same one.")

(defvar lyrion--lyrics-cache (make-hash-table :test #'equal)
  "Lyrics already looked up this session, by track; nil for none found.")

(defvar-local lyrion--lyrics-track nil
  "The track whose lyrics the buffer shows, as `lyrion--track' gives it.")

(defvar-local lyrion--lyrics-all nil
  "All the lyrics the buffer shows, empty lines included, or nil.")

(defvar-local lyrion--lyrics-lines nil
  "The lines shown, a vector of (TIME . TEXT); TIME is nil when untimed.")

(defvar-local lyrion--lyrics-positions nil
  "Where each of `lyrion--lyrics-lines' starts in the buffer, a vector.")

(defvar-local lyrion--lyrics-index nil
  "Index of the line lit as being sung, or nil.")

(defvar-local lyrion--lyrics-overlay nil
  "Overlay that lights the line being sung.")

(defvar-local lyrion--lyrics-timer nil
  "Timer that moves the light from line to line.")

(defun lyrion--track (status)
  "Return the track STATUS is on as (ARTIST TITLE ALBUM DURATION), or nil."
  (when-let* ((title (or (lyrion--get status "title")
                         (lyrion--get status "current_title"))))
    (list (lyrion--get status "artist") title (lyrion--get status "album")
          (lyrion--number (lyrion--get status "duration")))))

(defun lyrion--parse-lrc (text)
  "Return the timed lines of TEXT, in LRC form, as (TIME . TEXT), in order.
A line with several time stamps is sung at each; tags such as [ar:...] and
lines without a time are left out."
  (let (lines)
    (dolist (line (split-string text "\r?\n"))
      (let ((line (string-trim line))
            (start 0)
            (times nil))
        (while (and (string-match lyrion--lrc-time line start)
                    (= (match-beginning 0) start))
          (push (+ (* 60 (string-to-number (match-string 1 line)))
                   (string-to-number (match-string 2 line)))
                times)
          (setq start (match-end 0)))
        (let ((words (string-trim (substring line start))))
          (dolist (time times)
            (push (cons time words) lines)))))
    (sort lines (lambda (a b) (< (car a) (car b))))))

(defun lyrion--plain-lines (text)
  "Return the lines of TEXT, untimed lyrics, as (nil . TEXT)."
  (mapcar (lambda (line) (cons nil (string-trim-right line)))
          (split-string (string-trim text) "\r?\n")))

(defun lyrion--lyrics-from-entry (entry)
  "Return the lyrics of ENTRY, one LRCLIB record, or nil when it has none.
Timed lyrics are preferred to plain ones."
  (let ((synced (alist-get 'syncedLyrics entry))
        (plain (alist-get 'plainLyrics entry)))
    (cond ((null entry) nil)
          ((alist-get 'instrumental entry) (list (cons nil "Instrumental")))
          ((and (stringp synced) (not (string-blank-p synced)))
           (lyrion--parse-lrc synced))
          ((and (stringp plain) (not (string-blank-p plain)))
           (lyrion--plain-lines plain)))))

(defun lyrion--closest-entry (entries duration)
  "Return the entry among ENTRIES whose length is closest to DURATION, or nil.
One more than `lyrion--lyrics-tolerance' seconds off is not taken. With no
DURATION, the first entry with lyrics is."
  (let (best gap)
    (dolist (entry entries)
      (let ((length (alist-get 'duration entry)))
        (when (and (numberp length) (lyrion--lyrics-from-entry entry))
          (let ((distance (if duration (abs (- length duration)) 0)))
            (when (and (<= distance lyrion--lyrics-tolerance)
                       (or (null gap) (< distance gap)))
              (setq best entry gap distance))))))
    best))

(defun lyrion--lrclib (endpoint parameters callback)
  "Ask LRCLIB's ENDPOINT with PARAMETERS, then call CALLBACK.
PARAMETERS is an alist of names and values. CALLBACK receives (STATUS
. JSON), where JSON is the parsed body or nil, or nil when the request
failed."
  (let* ((url (url-generic-parse-url lyrion-lyrics-url))
         (tls (equal (url-type url) "https"))
         (path (concat (string-remove-suffix "/" (url-filename url)) endpoint
                       "?" (mapconcat (lambda (pair)
                                        (concat (car pair) "="
                                                (url-hexify-string (cdr pair))))
                                      parameters "&"))))
    (lyrion--http-get
     (url-host url) (url-port url) path
     (lambda (response)
       (funcall callback
                (when response
                  (cons (car response)
                        (ignore-errors
                          (json-parse-string
                           (decode-coding-string (cdr response) 'utf-8)
                           :object-type 'alist :array-type 'list
                           :null-object nil :false-object nil))))))
     tls)))

(defun lyrion--lyrics-lookup (track callback)
  "Look up TRACK's lyrics, then call CALLBACK with them.
CALLBACK receives the lines, nil when LRCLIB has none, or `failed' when it
could not be asked. Answers are kept for the session; failures are not."
  (let ((cached (gethash track lyrion--lyrics-cache 'unknown)))
    (if (not (eq cached 'unknown))
        (funcall callback cached)
      (pcase-let* ((`(,artist ,title ,album ,duration) track)
                   (finish (lambda (lyrics)
                             (puthash track lyrics lyrion--lyrics-cache)
                             (funcall callback lyrics))))
        (lyrion--lrclib
         "/get"
         (delq nil (list (cons "artist_name" (or artist ""))
                         (cons "track_name" title)
                         (and album (cons "album_name" album))
                         (and duration (cons "duration"
                                             (number-to-string
                                              (round duration))))))
         (lambda (answer)
           (pcase (car answer)
             (200 (funcall finish (lyrion--lyrics-from-entry (cdr answer))))
             ;; No exact match: search, and take the closest in length.
             (404
              (lyrion--lrclib
               "/search" (list (cons "artist_name" (or artist ""))
                               (cons "track_name" title))
               (lambda (results)
                 (if (eql (car results) 200)
                     (funcall finish (lyrion--lyrics-from-entry
                                      (lyrion--closest-entry (cdr results)
                                                             duration)))
                   (funcall callback 'failed)))))
             (_ (funcall callback 'failed)))))))))

(defun lyrion--lyrics-text (lyrics)
  "Return the words of LYRICS without their times, or nil when there are none.
Empty lines, where the music plays on, are left out."
  (when-let* ((words (seq-remove #'string-empty-p
                                 (mapcar (lambda (line)
                                           (string-trim (cdr line)))
                                         lyrics))))
    (concat (string-join words "\n") "\n")))

(defun lyrion--lyrics-index (lines elapsed)
  "Return the index of the line of LINES sung at ELAPSED seconds, or nil.
LINES is a vector of timed lines, in order."
  (let ((index nil)
        (time (+ elapsed lyrion--lyrics-lead)))
    (dotimes (i (length lines))
      (when (<= (car (aref lines i)) time)
        (setq index i)))
    index))

(defun lyrion--lyrics-render (track lyrics &optional notice)
  "Draw TRACK's LYRICS in the current buffer, or NOTICE in their place."
  (let* ((inhibit-read-only t)
         (shown (if lyrion-lyrics-hide-empty-lines
                    (seq-remove (lambda (line) (string-empty-p (cdr line)))
                                lyrics)
                  lyrics))
         (timed (and shown (car (car shown))))
         (positions nil))
    (erase-buffer)
    (insert "\n")
    (when track
      (insert (propertize (nth 1 track) 'face 'lyrion-lyrics-title) "\n")
      (when (nth 0 track)
        (insert (propertize (nth 0 track) 'face 'lyrion-album) "\n"))
      (insert "\n"))
    (if (or notice (null shown))
        (insert (propertize (or notice "No lyrics found") 'face 'lyrion-album)
                "\n")
      (dolist (line shown)
        (push (point) positions)
        (insert (if timed
                    (propertize (cdr line) 'face 'lyrion-lyrics)
                  (cdr line))
                "\n")))
    (setq lyrion--lyrics-all (and (not notice) lyrics)
          lyrion--lyrics-lines (and (not notice) timed (vconcat shown))
          lyrion--lyrics-positions (vconcat (nreverse positions))
          lyrion--lyrics-index nil)
    (move-overlay lyrion--lyrics-overlay (point-min) (point-min))
    (goto-char (point-min))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-start window (point-min)))))

(defun lyrion--lyrics-light (index)
  "Light line INDEX of the lyrics as being sung, and keep it in view."
  (let* ((start (aref lyrion--lyrics-positions index))
         (end (save-excursion (goto-char start) (line-end-position))))
    (move-overlay lyrion--lyrics-overlay start end)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      ;; Keep the sung line a third of the way down the window.
      (set-window-start
       window (save-excursion
                (goto-char start)
                (forward-line (- (/ (window-body-height window) 3)))
                (point))))))

(defun lyrion--lyrics-tick (buffer)
  "Light the line of BUFFER's lyrics being sung, while they are shown."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (when-let* ((lines lyrion--lyrics-lines)
                  (player (get-buffer lyrion-buffer-name))
                  (index (with-current-buffer player
                           (and lyrion--status
                                (lyrion--lyrics-index
                                 lines (lyrion--elapsed lyrion--status
                                                        lyrion--status-time
                                                        (float-time)))))))
        (unless (eql index lyrion--lyrics-index)
          (setq lyrion--lyrics-index index)
          (lyrion--lyrics-light index))))))

(defun lyrion--lyrics-load (buffer track &optional again)
  "Show TRACK's lyrics in BUFFER, looking them up when they are not known.
AGAIN non-nil looks them up even when BUFFER already shows TRACK."
  (with-current-buffer buffer
    (when (or again (not (equal track lyrion--lyrics-track)))
      (setq lyrion--lyrics-track track)
      (if (not track)
          (lyrion--lyrics-render nil nil "Nothing playing")
        (lyrion--lyrics-render track nil "Looking up the lyrics")
        (lyrion--lyrics-lookup
         track
         (lambda (lyrics)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               ;; The track may have changed while LRCLIB answered.
               (when (equal track lyrion--lyrics-track)
                 (if (eq lyrics 'failed)
                     (lyrion--lyrics-render track nil "Could not reach LRCLIB")
                   (lyrion--lyrics-render track lyrics)))))))))))

(defun lyrion--lyrics-follow (track)
  "Show TRACK's lyrics, when the lyrics are on screen."
  (when-let* ((buffer (get-buffer lyrion-lyrics-buffer-name)))
    (when (get-buffer-window buffer t)
      (lyrion--lyrics-load buffer track))))

(defun lyrion--lyrics-display-action ()
  "Return how the lyrics are shown: in a side window, as the options say."
  `(display-buffer-in-side-window
    (side . ,lyrion-lyrics-side)
    (,(if (memq lyrion-lyrics-side '(left right)) 'window-width 'window-height)
     . ,lyrion-lyrics-size)
    (dedicated . t)))

(defun lyrion--lyrics-close ()
  "Stop the current lyrics buffer's timer."
  (when (timerp lyrion--lyrics-timer)
    (cancel-timer lyrion--lyrics-timer)))

;;; The connection

(defun lyrion--send (words)
  "Send WORDS, one command, to the server."
  (when (process-live-p lyrion--process)
    (process-send-string lyrion--process (lyrion--encode words))))

(defun lyrion--ask-players-later (delay)
  "Ask the server for its players again after DELAY seconds."
  (when (timerp lyrion--retry) (cancel-timer lyrion--retry))
  (setq lyrion--retry
        (run-at-time delay nil
                     (lambda (buffer)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (setq lyrion--retry nil)
                           (lyrion--send '("players" "0" "99")))))
                     (current-buffer))))

(defun lyrion--handle-players (players)
  "Choose the player to show among PLAYERS and subscribe to its status.
This computer's player, just started, is waited for until it connects."
  (let* ((wanted (lyrion--wanted))
         (player (lyrion--choose-player players wanted))
         (id (lyrion--get player "playerid")))
    (cond
     ((and lyrion--local-until
           (< (float-time) lyrion--local-until)
           (equal wanted (lyrion--local-mac lyrion-squeezelite-name))
           (not (equal (lyrion--get player "connected") "1")))
      (setq lyrion--notice (format "Waiting for %s to connect"
                                   lyrion-squeezelite-name))
      (lyrion--render)
      (lyrion--ask-players-later lyrion--local-retry))
     ((not player)
      (setq lyrion--notice (if wanted
                               (format "No player called %s; looking again"
                                       wanted)
                             "The server has no players"))
      (lyrion--render)
      ;; It may be on its way, such as squeezelite after the server restarts.
      (when wanted
        (lyrion--ask-players-later lyrion-reconnect-delay)))
     (t
      (setq lyrion--player player
            lyrion--notice nil
            lyrion--local-until nil
            lyrion--transition nil)
      ;; The server sends the status now and again on every change.
      ;; Two tracks: the one playing, and the next, shown as up next.
      (lyrion--send (list id "status" "-" "2" lyrion--status-tags
                          "subscribe:0"))
      ;; Likewise the crossfade setting, which the status leaves out.
      (lyrion--send '("subscribe" "prefset"))
      (dolist (pref lyrion--transition-prefs)
        (lyrion--send (list id "playerpref" pref "?")))
      (when (and lyrion--local-volume-pending lyrion-local-volume)
        (lyrion--send (list id "mixer" "volume"
                            (number-to-string lyrion-local-volume))))
      (setq lyrion--local-volume-pending nil)))))

(defun lyrion--handle-status (status)
  "Show STATUS, the player's latest, fetching its cover when it is new."
  (let ((path (lyrion--cover-path status))
        (old lyrion--status))
    ;; A change of volume, from here or elsewhere, is said in the echo area.
    (let ((was (lyrion--get old "mixer volume"))
          (now (lyrion--get status "mixer volume")))
      (when (and was now (not (equal was now))
                 (equal (lyrion--get old "player_name")
                        (lyrion--get status "player_name")))
        (message "Volume %s%%" now)))
    (setq lyrion--status status
          lyrion--status-time (float-time))
    (unless (equal path (car lyrion--cover))
      ;; The last cover stays until the new one arrives, so the card keeps
      ;; its shape rather than closing up for a moment between albums.
      (setq lyrion--cover (and path (cons path (cdr lyrion--cover))))
      (when path (lyrion--fetch-cover path (current-buffer))))
    (lyrion--render)
    (lyrion--lyrics-follow (lyrion--track status))))

(defun lyrion--handle-pref (words)
  "Note the crossfade setting in WORDS, a player preference's value.
WORDS follow the player's id: `playerpref NAME VALUE', a reply, or
`prefset server NAME VALUE', a change made anywhere."
  (pcase-let ((`(,name ,value) (if (equal (car words) "prefset")
                                   (and (equal (cadr words) "server")
                                        (cddr words))
                                 (cdr words))))
    (when (and (member name lyrion--transition-prefs)
               value (not (equal value "?")))
      (setf (alist-get name lyrion--transition nil nil #'equal) value)
      (lyrion--render))))

(defun lyrion--handle (words)
  "Act on WORDS, one decoded line from the server."
  (let ((ours (and lyrion--player
                   (equal (car words) (lyrion--get lyrion--player "playerid")))))
    (cond
     ((equal (car words) "players")
      (lyrion--handle-players (lyrion--parse-players words)))
     ((and ours (equal (cadr words) "status"))
      (lyrion--handle-status (lyrion--parse-status words)))
     ((and ours (member (cadr words) '("playerpref" "prefset")))
      (lyrion--handle-pref (cdr words))))))

(defun lyrion--filter (process chunk buffer)
  "Handle CHUNK from PROCESS, the connection of BUFFER, line by line."
  (let ((lines (split-string (concat (or (process-get process 'pending) "")
                                     chunk)
                             "\n")))
    ;; The last piece is a line still on its way, or empty.
    (process-put process 'pending (car (last lines)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq process lyrion--process)
          (dolist (line (butlast lines))
            (unless (string-empty-p line)
              (lyrion--handle
               (lyrion--decode (string-trim-right line "\r"))))))))))

(defun lyrion--sentinel (process event buffer)
  "Handle EVENT on PROCESS, the connection of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; A connection replaced by a newer one has nothing more to say.
      (when (eq process lyrion--process)
        (cond
         ((string-prefix-p "open" event)
          (lyrion--send '("players" "0" "99")))
         ((not (process-live-p process))
          (setq lyrion--process nil
                lyrion--notice (format "Lost %s (%s); trying again"
                                       lyrion-host (string-trim event)))
          (lyrion--render)
          (setq lyrion--retry
                (run-at-time lyrion-reconnect-delay nil
                             #'lyrion--connect buffer))))))))

(defun lyrion--connect (buffer)
  "Connect BUFFER to the server, to choose a player and follow its status."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp lyrion--retry) (cancel-timer lyrion--retry))
      (setq lyrion--retry nil
            lyrion--notice (format "Connecting to %s" lyrion-host))
      (lyrion--render)
      (condition-case err
          (setq lyrion--process
                (make-network-process
                 :name "lyrion" :host lyrion-host :service lyrion-cli-port
                 :nowait t :coding 'binary :noquery t
                 :filter (lambda (process chunk)
                           (lyrion--filter process chunk buffer))
                 :sentinel (lambda (process event)
                             (lyrion--sentinel process event buffer))))
        (error
         (setq lyrion--notice (format "Cannot reach %s: %s; trying again"
                                      lyrion-host (error-message-string err))
               lyrion--retry (run-at-time lyrion-reconnect-delay nil
                                          #'lyrion--connect buffer))
         (lyrion--render))))))

(defun lyrion--close ()
  "Stop the timers and close the connection of the current buffer."
  (dolist (timer (list lyrion--timer lyrion--retry))
    (when (timerp timer) (cancel-timer timer)))
  (let ((process lyrion--process))
    (setq lyrion--process nil)
    (when (process-live-p process) (delete-process process)))
  ;; The lyrics follow the player, so they go with it.
  (when-let* ((lyrics (get-buffer lyrion-lyrics-buffer-name)))
    (kill-buffer lyrics)))

;;; The library, for searching

(defconst lyrion--library-queries
  '((track "id" "titles" "0" "100000" "tags:alde")
    (release "id" "albums" "0" "100000" "tags:lay")
    (artist "id" "artists" "0" "100000" "role_id:ALBUMARTIST")
    (genre "id" "genres" "0" "100000")
    (work "work_id" "works" "0" "100000")
    (playlist "id" "playlists" "0" "100000"))
  "The command that lists each kind of item in the library, as (KIND KEY .
WORDS). KEY is the field that starts each item in the reply.")

(defconst lyrion--kinds
  '((track "Tracks" "track" "nf-md-music_note" "♪" lyrion-icon-track)
    (release "Releases" "album" "nf-md-album" "◉" lyrion-icon-release)
    (artist "Album artists" "artist" "nf-md-account_music" "◆"
            lyrion-icon-artist)
    (work "Works" "work" "nf-md-music_clef_treble" "♫" lyrion-icon-work)
    (genre "Genres" "genre" "nf-md-tag_multiple" "#" lyrion-icon-genre)
    (playlist "Playlists" "playlist" "nf-md-playlist_music" "≡"
              lyrion-icon-playlist)
    (qobuz-artist "Qobuz artists" nil "nf-md-account_music" "◆"
                  lyrion-icon-qobuz)
    (qobuz-playlist "Qobuz playlists" nil "nf-md-playlist_music" "≡"
                    lyrion-icon-qobuz)
    (qobuz-release "Qobuz releases" nil "nf-md-album" "◉" lyrion-icon-qobuz)
    (qobuz-track "Qobuz tracks" nil "nf-md-music_note" "♪" lyrion-icon-qobuz))
  "The kinds of item a search finds, as (KIND HEADING ID ICON PLAIN FACE).
HEADING groups the results, ID names the kind in commands, ICON is a Nerd
Font icon's name, PLAIN the icon used without Nerd Fonts, and FACE its face.")

(defvar lyrion--query-timeout 30
  "Seconds to wait for the server to list a kind of item in the library.")

(defvar lyrion--quick-timeout 5
  "Seconds to wait for a short answer, such as the players or the queue.")

(defvar lyrion--qobuz-timeout 15
  "Seconds to wait for Qobuz, whom the server asks in turn.")

(defvar lyrion--wait-notice 1
  "Seconds of waiting for the server after which the echo area says so.")

(defvar lyrion--library nil
  "Items listed this session, as (KIND . ITEMS), each item an alist.")

(defvar lyrion--library-pending nil
  "Kinds being listed, as (KIND . CALLBACKS) waiting for the items.")

(defvar lyrion--candidate-cache nil
  "Completion candidates built this session, as (KIND TEXTS . TABLE).
TABLE maps each text, without properties, to (KIND . ITEM).")

(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function prescient-completion-sort "prescient")
(defvar prescient-sort-length-enable)
(defvar vertico-sort-function)

(defun lyrion--query (words callback &optional timeout)
  "Send WORDS, a query, on a connection of its own, then call CALLBACK.
CALLBACK receives the reply's words, or nil when there is no reply within
TIMEOUT seconds, by default `lyrion--query-timeout'. A connection of its
own keeps a long listing from holding up the player's status."
  (let* ((chunks nil)
         (done nil)
         (timer nil)
         (finish (lambda (reply)
                   (unless done
                     (setq done t)
                     (when timer (cancel-timer timer))
                     (funcall callback reply)))))
    (condition-case nil
        (let ((process
               (make-network-process
                :name "lyrion-query" :host lyrion-host :service lyrion-cli-port
                :nowait t :coding 'binary :noquery t
                :filter
                (lambda (process chunk)
                  (push chunk chunks)
                  ;; The reply is one line, however long.
                  (when (string-search "\n" chunk)
                    (let ((line (car (split-string
                                      (apply #'concat (nreverse chunks))
                                      "\n"))))
                      (funcall finish (lyrion--decode
                                       (string-trim-right line "\r")))
                      (delete-process process))))
                :sentinel
                (lambda (process event)
                  (cond ((string-prefix-p "open" event)
                         (process-send-string process (lyrion--encode words)))
                        ((not (process-live-p process))
                         (funcall finish nil)))))))
          (setq timer (run-at-time (or timeout lyrion--query-timeout) nil
                                   (lambda ()
                                     (funcall finish nil)
                                     (delete-process process)))))
      (error (funcall finish nil)))))

(defun lyrion--disconnected-p ()
  "Return non-nil when lyrion is open but its connection to the server is down.
The player's own connection is the first to know when the server goes."
  (when-let* ((buffer (get-buffer lyrion-buffer-name)))
    (not (process-live-p (buffer-local-value 'lyrion--process buffer)))))

(defun lyrion--check-connected ()
  "Signal that the server is not connected, when that is known.
Waiting for an answer then would only hold Emacs up for nothing."
  (when (lyrion--disconnected-p)
    (user-error "Not connected to %s" lyrion-host)))

(defun lyrion--wait (done &optional doing)
  "Wait for the server, until calling DONE gives non-nil.
Emacs waits meanwhile; \\[keyboard-quit] stops it. Once the wait has
lasted `lyrion--wait-notice' seconds, the echo area says what it is for:
DOING, a format string given the server's name, or the server itself."
  (let ((since (float-time))
        (said nil))
    (while (not (funcall done))
      (when (and (not said) (> (- (float-time) since) lyrion--wait-notice))
        (setq said t)
        (message "%s... (%s gives up)"
                 (format (or doing "Waiting for %s") lyrion-host)
                 (substitute-command-keys "\\[keyboard-quit]")))
      (accept-process-output nil 0.05))
    (when said (message nil))))

(defun lyrion--library-load (kind callback)
  "Call CALLBACK with the items of KIND, listing them once a session.
CALLBACK receives nil when they cannot be listed."
  (let ((listed (assq kind lyrion--library))
        (pending (assq kind lyrion--library-pending)))
    (cond
     (listed (funcall callback (cdr listed)))
     (pending (push callback (cdr pending)))
     (t
      (push (list kind callback) lyrion--library-pending)
      (pcase-let ((`(,key . ,words) (cdr (assq kind lyrion--library-queries))))
        (lyrion--query
         words
         (lambda (reply)
           (let ((items (and reply (lyrion--group reply key)))
               (callbacks (cdr (assq kind lyrion--library-pending))))
             (setq lyrion--library-pending
                   (assq-delete-all kind lyrion--library-pending))
             ;; An empty listing is kept too: there may be no playlists.
             (when reply
               (setf (alist-get kind lyrion--library) items))
             (dolist (waiting (reverse callbacks))
               (funcall waiting items))))))))))

(defun lyrion--library-prefetch ()
  "Start listing every kind of item not yet listed, without waiting."
  (dolist (kind (mapcar #'car lyrion--library-queries))
    (lyrion--library-load kind #'ignore)))

(defun lyrion--recordings (works releases)
  "Return WORKS as recordings: one item per work and release it is on.
Each takes its release's name and album artist from RELEASES."
  (let ((by-id (make-hash-table :test #'equal :size (length releases))))
    (dolist (release releases)
      (puthash (lyrion--get release "id") release by-id))
    (mapcan (lambda (work)
              (mapcar (lambda (album-id)
                        (let ((release (gethash album-id by-id)))
                          (append
                           (seq-remove (lambda (field)
                                         (equal (car field) "album_id"))
                                       work)
                           (list (cons "album_id" album-id)
                                 (cons "album" (lyrion--get release "album"))
                                 (cons "artist"
                                       (lyrion--get release "artist"))))))
                      (split-string (or (lyrion--get work "album_id") "")
                                    "," t)))
            works)))

(defun lyrion--library-items (kinds)
  "Return the items of KINDS as (KIND . ITEMS), waiting for those not listed.
Works come as recordings, which need the releases listed too."
  (let* ((wanted (if (and (memq 'work kinds) (not (memq 'release kinds)))
                     (cons 'release kinds)
                   kinds))
         (results nil)
         (left (length wanted)))
    (unless (seq-every-p (lambda (kind) (assq kind lyrion--library)) wanted)
      (lyrion--check-connected))
    (dolist (kind wanted)
      (lyrion--library-load kind (lambda (items)
                                   (push (cons kind items) results)
                                   (setq left (1- left)))))
    (unless (zerop left)
      (message "Listing the library on %s..." lyrion-host)
      (lyrion--wait (lambda () (zerop left)) "Listing the library on %s")
      (message nil))
    (dolist (kind wanted)
      (unless (assq kind lyrion--library)
        (user-error "Could not list the %s on %s"
                    (downcase (nth 1 (assq kind lyrion--kinds))) lyrion-host)))
    (mapcar (lambda (kind)
              (cons kind (if (eq kind 'work)
                             (lyrion--recordings (alist-get 'work results)
                                                 (alist-get 'release results))
                           (alist-get kind results))))
            kinds)))

(defun lyrion--with-detail (main details)
  "Return MAIN followed by DETAILS, a list of strings, shown dimmed.
The details are drawn against the window's right edge, but stay part of
the text, so they match. Their width is counted in columns, not pixels,
which would be slow over a whole library."
  (let ((details (seq-remove (lambda (detail)
                               (or (null detail) (string-empty-p detail)
                                   (equal detail "0")))
                             details)))
    (concat (or main "")
            (when details
              (let ((detail (propertize (string-join details " · ")
                                        'face 'lyrion-search-detail)))
                ;; One plain space keeps them apart when the line is too long
                ;; to align; the next stretches to the right edge.
                (concat " "
                        (propertize " " 'display
                                    `(space :align-to
                                            (- right ,(1+ (string-width
                                                           detail)))))
                        detail))))))

(defun lyrion--item-name (kind item)
  "Return the name of ITEM, an item of KIND."
  (lyrion--get item (pcase kind
                      ('track "title")
                      ('release "album")
                      ('artist "artist")
                      ('genre "genre")
                      ('work "work")
                      ('playlist "playlist")
                      ('qobuz-release "album")
                      ('qobuz-track "title")
                      ;; Qobuz artists and playlists.
                      (_ "name"))))

(defun lyrion--candidate-text (kind item)
  "Return ITEM, an item of KIND, as the text of a search result.
The details are part of it, so they can be matched too."
  (let ((get (lambda (key) (lyrion--get item key))))
    (pcase kind
      ((or 'track 'qobuz-track)
       (lyrion--with-detail (funcall get "title")
                            (list (funcall get "artist")
                                  (funcall get "album"))))
      ((or 'release 'qobuz-release)
       (lyrion--with-detail (funcall get "album")
                            (list (funcall get "artist")
                                  (funcall get "year"))))
      ('artist (lyrion--with-detail (funcall get "artist") nil))
      ('genre (lyrion--with-detail (funcall get "genre") nil))
      ('playlist (lyrion--with-detail (funcall get "playlist") nil))
      ((or 'qobuz-artist 'qobuz-playlist)
       (lyrion--with-detail (funcall get "name") nil))
      ('work (lyrion--with-detail (funcall get "work")
                                  (list (funcall get "composer")
                                        (funcall get "album")
                                        (unless (equal (funcall get "artist")
                                                       (funcall get "composer"))
                                          (funcall get "artist"))))))))

(defun lyrion--candidates-of (kind items)
  "Return the search results for ITEMS, of KIND, as (TEXTS . TABLE).
TEXTS are in the order of ITEMS. Two results with the same text are told
apart by a number."
  (let ((table (make-hash-table :test #'equal :size (length items)))
        (texts nil))
    (dolist (item items)
      (let* ((base (lyrion--candidate-text kind item))
             (text base)
             (copy 1))
        (while (gethash (substring-no-properties text) table)
          (setq copy (1+ copy)
                text (format "%s (%d)" base copy)))
        (puthash (substring-no-properties text) (cons kind item) table)
        (push text texts)))
    (cons (nreverse texts) table)))

(defun lyrion--candidates (kind items)
  "Return the search results for ITEMS, of KIND, as (TEXTS . TABLE).
They are built once a session, in alphabetical order regardless of case."
  (or (alist-get kind lyrion--candidate-cache)
      (setf (alist-get kind lyrion--candidate-cache)
            (let ((set (lyrion--candidates-of kind items)))
              (cons (sort (car set)
                          (lambda (a b) (string-collate-lessp a b nil t)))
                    (cdr set))))))

(defun lyrion--icon (kind)
  "Return the icon of KIND: a Nerd Font one when `nerd-icons' is to hand."
  (pcase-let ((`(,_heading ,_id ,name ,plain ,face)
               (cdr (assq kind lyrion--kinds))))
    (if (or (fboundp 'nerd-icons-mdicon) (require 'nerd-icons nil t))
        (nerd-icons-mdicon name :face face)
      (propertize plain 'face face))))

(defvar lyrion--shuffle-chosen nil
  "Non-nil when the search result was accepted to be played shuffled.")

(defun lyrion-search-shuffle-exit ()
  "Accept the search result as RET would, and play it shuffled."
  (interactive)
  (setq lyrion--shuffle-chosen t)
  (call-interactively (key-binding (kbd "RET"))))

(defvar lyrion--open-chosen nil
  "Non-nil when the search result was accepted to be opened, not played.")

(defun lyrion-search-open-exit ()
  "Accept the search result as RET would, but open it rather than play it.
An artist opens into their releases, to choose one of them; anything else
plays as it would with RET."
  (interactive)
  (setq lyrion--open-chosen t)
  (call-interactively (key-binding (kbd "RET"))))

(defvar-keymap lyrion-search-minibuffer-map
  :doc "Keys added to the minibuffer while searching.
RET plays the result in order, S-RET plays it shuffled, and M-RET opens
an artist into their releases."
  "S-<return>" #'lyrion-search-shuffle-exit
  "M-<return>" #'lyrion-search-open-exit
  ;; What a terminal sends for M-RET.
  "M-RET" #'lyrion-search-open-exit)

(defun lyrion--ui-sort (candidates)
  "Sort CANDIDATES by what was chosen recently and often, then alphabetically.
That is Prescient's sort without its length rule, which means little for
music; with another sort, CANDIDATES keep their alphabetical order."
  (if (and (boundp 'vertico-sort-function)
           (eq vertico-sort-function 'prescient-completion-sort)
           (fboundp 'prescient-completion-sort))
      (let ((prescient-sort-length-enable nil))
        (prescient-completion-sort candidates))
    candidates))

(defun lyrion--read-item (kinds prompt &optional sets)
  "Read an item of KINDS, asking for PROMPT, and return it as (KIND . ITEM).
Results show their kind's icon, and are grouped by kind when there are
several. SETS, results as from `lyrion--candidates-of', take the place of
the library's, and keep their order, such as a search's relevance.
\\<lyrion-search-minibuffer-map>
\\[lyrion-search-shuffle-exit] accepts a result to be played shuffled, and
\\[lyrion-search-open-exit] one to be opened, as an artist opens into their
releases. The prompt says so, naming the latter only when there are
artists among KINDS."
  (let* ((ordered sets)
         (sets (or sets
                   (mapcar (lambda (entry)
                             (lyrion--candidates (car entry) (cdr entry)))
                           (lyrion--library-items kinds))))
         (texts (apply #'append (mapcar #'car sets)))
         (lookup (lambda (text)
                   (let ((key (substring-no-properties text)))
                     (seq-some (lambda (set) (gethash key (cdr set))) sets))))
         (affix (lambda (candidates)
                  (mapcar (lambda (candidate)
                            (list candidate
                                  (concat (lyrion--icon
                                           (car (funcall lookup candidate)))
                                          " ")
                                  ""))
                          candidates)))
         (group (lambda (candidate transform)
                  (if transform
                      candidate
                    (nth 1 (assq (car (funcall lookup candidate))
                                 lyrion--kinds)))))
         ;; Recent choices first, then the alphabet; groups come in the order
         ;; of KINDS, each sorted so within.
         (sort (lambda (candidates)
                 (let ((sorted (if ordered
                                   candidates
                                 (lyrion--ui-sort candidates))))
                   (if (cdr kinds)
                       (seq-sort-by (lambda (candidate)
                                      (seq-position
                                       kinds (car (funcall lookup candidate))))
                                    #'< sorted)
                     sorted))))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      `(metadata (category . lyrion-item)
                                 (affixation-function . ,affix)
                                 (display-sort-function . ,sort)
                                 ,@(and (cdr kinds)
                                        `((group-function . ,group))))
                    (complete-with-action action texts string predicate)))))
    (or (funcall lookup
                 (minibuffer-with-setup-hook
                     (:append (lambda ()
                                (use-local-map
                                 (make-composed-keymap
                                  lyrion-search-minibuffer-map
                                  (current-local-map)))))
                   (completing-read
                    (format (if (seq-intersection kinds '(artist qobuz-artist))
                                "%s (S-RET shuffles, M-RET opens an artist): "
                              "%s (S-RET shuffles): ")
                            prompt)
                    table nil t)))
        (user-error "Nothing chosen"))))

(defun lyrion--play-item (kind item &optional shuffle)
  "Play ITEM, an item of KIND, in place of the queue, shuffled when SHUFFLE.
Shuffling is the player's own mode, so it is set either way: otherwise one
shuffled choice would leave every later one shuffled too."
  (lyrion--player-command (list "playlist" "shuffle" (if shuffle "1" "0")))
  (lyrion--player-command
   (pcase kind
     ((or 'qobuz-release 'qobuz-track 'qobuz-playlist)
      (list "qobuz" "playlist" "play"
            (concat "item_id:" (lyrion--get item "id"))))
     ('qobuz-artist
      ;; The second entry of an artist's menu lists their songs: those of
      ;; Qobuz's first 200 tracks matching their name that they perform.
      (list "qobuz" "playlist" "play"
            (concat "item_id:" (lyrion--get item "id") ".1")))
     ('work
      ;; The work as recorded on one release, not every recording.
      (list "playlistcontrol" "cmd:load"
            (concat "work_id:" (lyrion--get item "work_id"))
            (concat "album_id:" (lyrion--get item "album_id"))))
     (_
      (list "playlistcontrol" "cmd:load"
            (format "%s_id:%s" (nth 2 (assq kind lyrion--kinds))
                    (lyrion--get item "id"))))))
  (message "%s %s" (if shuffle "Shuffling" "Playing")
           (lyrion--item-name kind item)))

(defun lyrion--artist-releases (artist)
  "Return the library's releases by ARTIST, an album artist, oldest first."
  (lyrion--group (lyrion--query-wait
                  (list "albums" "0" "100000"
                        (concat "artist_id:" (lyrion--get artist "id"))
                        "sort:yearalbum" "tags:lay"))
                 "id"))

(defun lyrion--read-release-by (name kind releases)
  "Read one of RELEASES, of KIND, by the artist NAME, as (KIND . ITEM).
They keep their order. The keys start afresh: RET, S-RET and M-RET then
choose how the release plays."
  (unless releases
    (user-error "No releases by %s" name))
  (setq lyrion--shuffle-chosen nil
        lyrion--open-chosen nil)
  (lyrion--read-item (list kind) (format "Release by %s" name)
                     (list (lyrion--candidates-of kind releases))))

(defun lyrion--search (kinds prompt &optional sets)
  "Read an item of KINDS, asking for PROMPT, and play it.
RET plays it in order and S-RET shuffled. M-RET opens an album artist
into their releases, to choose one; anything else it plays as RET does.
SETS are as for `lyrion--read-item'."
  (let ((lyrion--shuffle-chosen nil)
        (lyrion--open-chosen nil))
    (pcase-let ((`(,kind . ,item) (lyrion--read-item kinds prompt sets)))
      (when (and lyrion--open-chosen (eq kind 'artist))
        (pcase-setq `(,kind . ,item)
                    (lyrion--read-release-by (lyrion--item-name kind item)
                                             'release
                                             (lyrion--artist-releases item))))
      (lyrion--play-item kind item lyrion--shuffle-chosen))))

;;; Plugins

(defconst lyrion--plugins '("qobuz" "radioparadise")
  "The server plugins lyrion uses when they are there, by their commands.")

(defvar lyrion--plugins-found nil
  "The plugins asked about this session, as (COMMAND . THERE).")

(defvar lyrion--plugins-timeout 2
  "Seconds to wait for the server to say which plugins it has.")

(defun lyrion--check-plugins ()
  "Ask the server once a session which of `lyrion--plugins' it has.
The answers are waited for, briefly, since the search menu shows a plugin's
entry only when it is there. One not answered is asked about next time.
With the connection down, nothing is asked, so the menu opens at once."
  (let* ((asking (unless (lyrion--disconnected-p)
                   (seq-remove (lambda (command)
                                 (assoc command lyrion--plugins-found))
                               lyrion--plugins)))
         (left (length asking))
         (until (+ (float-time) lyrion--plugins-timeout)))
    (dolist (command asking)
      (lyrion--query (list "can" command "items" "?")
                     (lambda (reply)
                       (setq left (1- left))
                       (when reply
                         (setf (alist-get command lyrion--plugins-found
                                          nil nil #'equal)
                               (equal (car (last reply)) "1"))))
                     lyrion--quick-timeout))
    (while (and (> left 0) (< (float-time) until))
      (accept-process-output nil 0.05))))

(defun lyrion--plugin-p (command)
  "Return non-nil when the server is known to have the plugin COMMAND."
  (alist-get command lyrion--plugins-found nil nil #'equal))

(defun lyrion--qobuz-p ()
  "Return non-nil when the server is known to have the Qobuz plugin."
  (lyrion--plugin-p "qobuz"))

(defun lyrion--radio-paradise-p ()
  "Return non-nil when the server is known to have the Radio Paradise plugin."
  (lyrion--plugin-p "radioparadise"))

;;; Radio Paradise

(defun lyrion--plugin-items (command &optional item-id)
  "Return the items of plugin COMMAND's menu, or of its entry ITEM-ID."
  (lyrion--group (lyrion--query-wait
                  (append (list (lyrion--player-id) command "items" "0" "100")
                          (when item-id
                            (list (concat "item_id:" item-id)))))
                 "id"))

(defun lyrion--read-in-order (prompt names)
  "Read one of NAMES, asking for PROMPT, offering them in their own order."
  (completing-read prompt
                   (lambda (string predicate action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity))
                       (complete-with-action action names string predicate)))
                   nil t))

(defun lyrion-radio-paradise ()
  "Choose a Radio Paradise channel and play it in place of the queue.
A channel with several streams plays its first, the plugin's best: FLAC,
which lets the server skip songs."
  (interactive)
  (let* ((channels (lyrion--plugin-items "radioparadise"))
         (name (lyrion--read-in-order
                "Radio Paradise channel: "
                (mapcar (lambda (channel) (lyrion--get channel "name"))
                        channels)))
         (channel (seq-find (lambda (channel)
                              (equal (lyrion--get channel "name") name))
                            channels))
         (stream (if (equal (lyrion--get channel "hasitems") "1")
                     (seq-find (lambda (item)
                                 (equal (lyrion--get item "isaudio") "1"))
                               (lyrion--plugin-items
                                "radioparadise" (lyrion--get channel "id")))
                   channel)))
    (unless stream
      (user-error "Radio Paradise has no stream for %s" name))
    (lyrion--player-command
     (list "radioparadise" "playlist" "play"
           (concat "item_id:" (lyrion--get stream "id"))))
    (message "Playing %s" (lyrion--get stream "name"))))

;;; Qobuz

(defun lyrion--qobuz-release (item)
  "Return ITEM, a release from a Qobuz search, with its name taken apart.
The plugin names a release \"ARTIST - TITLE (YEAR)\", the year on a line
of its own, which decoding turns into a space; the parts are its
\"artist\", \"album\" and \"year\"."
  (let ((name (or (lyrion--get item "name") "")))
    (append item
            (if (string-match (concat "\\`\\(.*?\\) - \\(.*?\\)"
                                      "\\(?:[ \n](\\([0-9]+\\))\\)?\\'")
                              name)
                (list (cons "artist" (match-string 1 name))
                      (cons "album" (match-string 2 name))
                      (cons "year" (match-string 3 name)))
              (list (cons "album" name))))))

(defun lyrion--qobuz-track (item)
  "Return ITEM, a track from a Qobuz search, with its name taken apart.
The plugin names a track \"TITLE by ARTIST from RELEASE\"; the parts are
its \"title\", \"artist\" and \"album\". Titles such as Stand by Me
contain \" by \" too, so the title runs to the last one that a \" from \"
follows."
  (let ((name (string-trim (or (lyrion--get item "name") ""))))
    (append item
            (if (string-match "\\`\\(.*\\) by \\(.*?\\) from \\(.*\\)\\'" name)
                (list (cons "title" (match-string 1 name))
                      (cons "artist" (match-string 2 name))
                      (cons "album" (match-string 3 name)))
              (list (cons "title" name))))))

(defconst lyrion--qobuz-branches
  '((qobuz-release . "0") (qobuz-artist . "1") (qobuz-track . "2")
    (qobuz-playlist . "3"))
  "Where each kind of item sits in the plugin's search menu.")

(defun lyrion--qobuz-items (kind words)
  "Return the items of KIND that WORDS, a reply from the plugin, lists.
Headings among them, such as an artist's \"Albums (31)\", are left out."
  (mapcar (lambda (item)
            (pcase kind
              ('qobuz-release (lyrion--qobuz-release item))
              ('qobuz-track (lyrion--qobuz-track item))
              (_ item)))
          (seq-remove (lambda (item) (equal (lyrion--get item "type") "header"))
                      (lyrion--group words "id"))))

(defun lyrion--query-all (queries &optional timeout)
  "Send QUERIES, each a list of words, at once, and wait for every reply.
Return the replies in the order of QUERIES, nil for one not answered within
TIMEOUT seconds, by default `lyrion--quick-timeout'."
  (lyrion--check-connected)
  (let ((replies (make-vector (length queries) 'waiting))
        (index 0))
    (dolist (words queries)
      (let ((slot index))
        (lyrion--query words (lambda (reply) (aset replies slot reply))
                       (or timeout lyrion--quick-timeout)))
      (setq index (1+ index)))
    (lyrion--wait (lambda () (not (seq-contains-p replies 'waiting))))
    (append replies nil)))

(defun lyrion--qobuz-search (term kinds)
  "Search Qobuz for items of KINDS matching TERM, as (KIND . ITEMS).
Each kind is a search of its own, sent at once."
  (let* ((id (lyrion--player-id))
         (replies
          (lyrion--query-all
           (mapcar (lambda (kind)
                     (list id "qobuz" "items" "0"
                           (number-to-string lyrion-qobuz-results)
                           ;; The kind's results, without opening the menu.
                           (concat "item_id:0.0."
                                   (alist-get kind lyrion--qobuz-branches))
                           (concat "search:" term)))
                   kinds)
           lyrion--qobuz-timeout)))
    (when (memq nil replies)
      (user-error "%s did not answer the search" lyrion-host))
    (seq-mapn (lambda (kind reply)
                (cons kind (lyrion--qobuz-items kind reply)))
              kinds replies)))

(defun lyrion--qobuz-artist-releases (artist)
  "Return the releases of ARTIST, an artist from a Qobuz search."
  (lyrion--qobuz-items
   'qobuz-release
   (lyrion--query-wait
    (list (lyrion--player-id) "qobuz" "items" "0"
          (number-to-string lyrion-qobuz-results)
          ;; The first entry of an artist's menu lists their releases.
          (concat "item_id:" (lyrion--get artist "id") ".0"))
    nil lyrion--qobuz-timeout)))

;;; What a search includes

(defconst lyrion--categories
  '(("artists" artist qobuz-artist)
    ("genres" genre nil)
    ("playlists" playlist qobuz-playlist)
    ("releases" release qobuz-release)
    ("works" work nil)
    ("tracks" track qobuz-track))
  "The categories a search can include, as (SWITCH KIND QOBUZ-KIND).
SWITCH is the search menu's, and the kinds are what it includes from the
library and from Qobuz, nil for none. Results are grouped in this order.")

(defun lyrion--chosen-categories ()
  "Return the categories chosen in the search menu, as its switch arguments.
Outside the menu, they are the ones it opens with: those set in it this
session with `transient-set', else those saved with `transient-save', else
every one."
  (if (eq transient-current-command 'lyrion-search-menu)
      (transient-args 'lyrion-search-menu)
    (let ((menu (clone (get 'lyrion-search-menu 'transient--prefix))))
      (transient-init-value menu)
      (oref menu value))))

(defun lyrion--chosen-kinds (qobuz)
  "Return the kinds of item the chosen categories include, in their order.
QOBUZ non-nil means the kinds from Qobuz, otherwise from the library."
  (let ((chosen (lyrion--chosen-categories)))
    (or (delq nil (mapcar (lambda (category)
                            (and (member (car category) chosen)
                                 (nth (if qobuz 2 1) category)))
                          lyrion--categories))
        (user-error "No category chosen%s; switch one on in the menu"
                    (if qobuz " that Qobuz has" "")))))

;;; Commands

(defvar-keymap lyrion-mode-map
  :doc "Keymap of `lyrion-mode'."
  :parent special-mode-map
  "SPC" #'lyrion-play-pause
  "<" #'lyrion-previous
  ">" #'lyrion-next
  "<left>" #'lyrion-skip-back
  "<right>" #'lyrion-skip-forward
  "+" #'lyrion-volume-up
  ;; The same key as +, without shift.
  "=" #'lyrion-volume-up
  "-" #'lyrion-volume-down
  "p" #'lyrion-select-player
  "S-<return>" #'lyrion-shuffle
  "S" #'lyrion-shuffle-toggle
  "O" #'lyrion-play-in-order
  "R" #'lyrion-repeat
  "X" #'lyrion-crossfade
  "/" #'lyrion-search
  "L" #'lyrion-lyrics
  "C" #'lyrion-lyrics-copy
  "w" #'lyrion-copy-track
  "Z" #'lyrion-sleep
  "I" #'lyrion-queue
  "?" #'lyrion-dispatch
  "q" #'lyrion-quit)

(defvar-keymap lyrion-lyrics-mode-map
  :doc "Keymap of `lyrion-lyrics-mode'.
The player's keys, but for \\`q', which hides the lyrics alone."
  :parent lyrion-mode-map
  "q" #'quit-window)

(define-derived-mode lyrion-mode special-mode "Lyrion"
  "Show what a Lyrion Music Server player is playing.
\\<lyrion-mode-map>\\[lyrion-play-pause] plays or pauses.
\\[lyrion-shuffle] shuffles the queue afresh and plays it from the top.
\\[lyrion-shuffle-toggle] turns shuffle on or off, the music carrying on.
\\[lyrion-play-in-order] plays the queue in order from its first track.
\\[lyrion-repeat] changes the repeat mode.
\\[lyrion-volume-up] and \\[lyrion-volume-down] turn the volume up and down.
\\[lyrion-crossfade] turns smart crossfading on or off.
\\[lyrion-search] searches the library and plays what is chosen.
\\[lyrion-queue] shows the queue and plays the track chosen from it.
\\[lyrion-lyrics] shows the lyrics beside it.
\\[lyrion-lyrics-copy] copies them, while they are showing.
\\[lyrion-copy-track] copies the track as \"Artist - Release - Title\".
\\[lyrion-sleep] sets the sleep timer.
\\[lyrion-dispatch] shows every key in a menu.
\\[lyrion-quit] hides the lyrics and the player; playback goes on.
\\[revert-buffer] connects again, chooses the player afresh and lists the
library anew."
  (setq-local cursor-type nil
              truncate-lines t
              revert-buffer-function (lambda (&rest _) (lyrion-reconnect)))
  (buffer-disable-undo)
  (add-hook 'kill-buffer-hook #'lyrion--close nil t)
  (setq lyrion--timer (run-with-timer 1 1 #'lyrion--tick (current-buffer))))

(define-derived-mode lyrion-lyrics-mode special-mode "Lyrion lyrics"
  "Show the lyrics of the track a Lyrion player is playing.
Timed lyrics light the line being sung and keep it in view.
\\<lyrion-lyrics-mode-map>\\[lyrion-play-pause] plays or pauses.
\\[lyrion-lyrics-copy] copies the words, without their times.
\\[lyrion-lyrics] or \\[quit-window] hides them.
\\[revert-buffer] looks them up again."
  (setq-local cursor-type nil
              word-wrap t
              truncate-lines nil
              left-margin-width 2
              right-margin-width 2
              revert-buffer-function
              (lambda (&rest _)
                (lyrion--lyrics-load (current-buffer) lyrion--lyrics-track t)))
  (buffer-disable-undo)
  (setq lyrion--lyrics-overlay (make-overlay (point-min) (point-min)))
  (overlay-put lyrion--lyrics-overlay 'face 'lyrion-lyrics-current)
  (add-hook 'kill-buffer-hook #'lyrion--lyrics-close nil t)
  (setq lyrion--lyrics-timer
        (run-with-timer 0.25 0.25 #'lyrion--lyrics-tick (current-buffer))))

;;;###autoload
(defun lyrion ()
  "Show what the Lyrion Music Server player is playing."
  (interactive)
  (let ((buffer (get-buffer-create lyrion-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lyrion-mode)
        (lyrion-mode))
      (when (equal (lyrion--wanted) (lyrion--local-mac lyrion-squeezelite-name))
        (lyrion--start-local buffer))
      (unless (or (process-live-p lyrion--process) (timerp lyrion--retry))
        (lyrion--connect buffer)))
    (pop-to-buffer-same-window buffer)))

(defun lyrion--player-buffer ()
  "Return the player's buffer, or signal that lyrion is not open."
  (or (get-buffer lyrion-buffer-name)
      (user-error "Lyrion is not open")))

(defun lyrion--restart (buffer)
  "Connect BUFFER to the server again and choose its player afresh."
  (with-current-buffer buffer
    (let ((process lyrion--process))
      (setq lyrion--process nil
            lyrion--player nil
            lyrion--status nil
            lyrion--cover nil)
      (when (process-live-p process) (delete-process process)))
    (lyrion--connect buffer)))

(defun lyrion-reconnect ()
  "Connect to the server again, choosing the player afresh.
A player chosen with `lyrion-select-player' stays chosen. The library is
listed anew at the next search."
  (interactive)
  (let ((buffer (lyrion--player-buffer)))
    (setq lyrion--library nil
          lyrion--candidate-cache nil
          lyrion--plugins-found nil)
    (lyrion--restart buffer)))

(defun lyrion--query-wait (words &optional failure timeout)
  "Send WORDS, a query, and return the reply's words, waiting for them.
Without a reply within TIMEOUT seconds, by default `lyrion--quick-timeout',
signal FAILURE, a format string given the server's name."
  (lyrion--check-connected)
  (let ((reply 'waiting))
    (lyrion--query words (lambda (words) (setq reply words))
                   (or timeout lyrion--quick-timeout))
    (lyrion--wait (lambda () (not (eq reply 'waiting))))
    (or reply (user-error (or failure "%s did not answer") lyrion-host))))

(defun lyrion--player-choice (player)
  "Return PLAYER as a choice to offer: its name and whether it is playing."
  (concat (lyrion--get player "name") " · "
          (cond ((not (equal (lyrion--get player "connected") "1"))
                 "not connected")
                ((equal (lyrion--get player "isplaying") "1") "playing")
                (t "connected"))))

(defun lyrion-select-player ()
  "Choose the server's player for lyrion to show and control.
This computer's player is always offered, and choosing it starts squeezelite.
The choice lasts while lyrion is open; `lyrion-player' sets the first."
  (interactive)
  (let* ((buffer (lyrion--player-buffer))
         (local (lyrion--local-mac lyrion-squeezelite-name))
         (players (lyrion--parse-players
                   (lyrion--query-wait '("players" "0" "99"))))
         (choices (mapcar (lambda (player)
                            (cons (lyrion--player-choice player) player))
                          players)))
    (unless (seq-find (lambda (player)
                        (equal (lyrion--get player "playerid") local))
                      players)
      (setq choices
            (append choices
                    (list (cons (concat lyrion-squeezelite-name
                                        " · this computer")
                                (list (cons "playerid" local)
                                      (cons "name"
                                            lyrion-squeezelite-name)))))))
    (let ((choice (cdr (assoc (completing-read "Player: " choices nil t)
                              choices))))
      (unless choice
        (user-error "No player chosen"))
      (when (equal (lyrion--get choice "playerid") local)
        (lyrion--start-local buffer))
      (with-current-buffer buffer
        (setq lyrion--chosen (lyrion--get choice "playerid")))
      (lyrion--restart buffer))))

;;;###autoload
(defun lyrion-local-player-stop ()
  "Stop the squeezelite lyrion started, so this computer is no longer a player."
  (interactive)
  (if (not (process-live-p lyrion--squeezelite))
      (message "Squeezelite is not running")
    (delete-process lyrion--squeezelite)
    (message "Stopped squeezelite")))

(defun lyrion--player-id ()
  "Return the id of the player shown, or signal that there is none."
  (let ((buffer (get-buffer lyrion-buffer-name)))
    (unless (and buffer (buffer-local-value 'lyrion--player buffer))
      (user-error "No Lyrion player is shown"))
    (lyrion--get (buffer-local-value 'lyrion--player buffer) "playerid")))

(defun lyrion--player-command (words)
  "Send WORDS, a command, to the player shown, whichever buffer is current.
WORDS may be a function of the player's status returning them."
  (lyrion--player-id)
  (let ((buffer (get-buffer lyrion-buffer-name)))
    (with-current-buffer buffer
      (unless (process-live-p lyrion--process)
        (user-error "Not connected to %s" lyrion-host))
      (lyrion--send (cons (lyrion--get lyrion--player "playerid")
                          (if (functionp words)
                              (funcall words lyrion--status)
                            words))))))

(defun lyrion-previous ()
  "Go back to the previous track in the player's queue."
  (interactive)
  (lyrion--player-command '("playlist" "index" "-1")))

(defun lyrion-next ()
  "Go on to the next track in the player's queue."
  (interactive)
  (lyrion--player-command '("playlist" "index" "+1")))

(defun lyrion-skip-back ()
  "Skip back `lyrion-skip-seconds' seconds in the track."
  (interactive)
  (lyrion--player-command (list "time" (format "-%s" lyrion-skip-seconds))))

(defun lyrion-skip-forward ()
  "Skip forward `lyrion-skip-seconds' seconds in the track."
  (interactive)
  (lyrion--player-command (list "time" (format "+%s" lyrion-skip-seconds))))

(defun lyrion-volume-up ()
  "Turn the player's volume up by `lyrion-volume-step'."
  (interactive)
  (lyrion--player-command
   (list "mixer" "volume" (format "+%s" lyrion-volume-step))))

(defun lyrion-volume-down ()
  "Turn the player's volume down by `lyrion-volume-step'."
  (interactive)
  (lyrion--player-command
   (list "mixer" "volume" (format "-%s" lyrion-volume-step))))

(defun lyrion-play-pause ()
  "Pause the player shown, or play it when it is paused or stopped.
The server sends the new status, which the player then shows."
  (interactive)
  (lyrion--player-command
   (lambda (status)
     ;; `pause' toggles between playing and paused, but a stopped player
     ;; needs `play'.
     (if (equal (lyrion--get status "mode") "stop") '("play") '("pause")))))

(defun lyrion-shuffle ()
  "Shuffle the player's queue afresh and play it from the top.
The server keeps the track playing first in a new order, so play moves on
to the second, the first the shuffle picked, and the queue is shuffled
again around that one: it then plays first, with the track left behind
somewhere among the rest. Paused or stopped, the player starts playing."
  (interactive)
  (lyrion--player-command '("playlist" "shuffle" "1"))
  (lyrion--player-command '("playlist" "index" "1"))
  (lyrion--player-command '("playlist" "shuffle" "1")))

(defun lyrion-play-in-order ()
  "Turn shuffle off and play the player's queue from its first track."
  (interactive)
  (lyrion--player-command '("playlist" "shuffle" "0"))
  (lyrion--player-command '("playlist" "index" "0")))

(defun lyrion-shuffle-toggle ()
  "Turn shuffle on or off, leaving the track playing where it is.
On, the server shuffles the queue around the track playing; off, it puts
the queue back in order. Either way the music carries on."
  (interactive)
  (lyrion--player-command
   (lambda (status)
     (list "playlist" "shuffle"
           (if (member (lyrion--get status "playlist shuffle") '("1" "2"))
               "0"
             "1")))))

(defun lyrion-repeat ()
  "Change the player's repeat mode: off, then the track, then all of the queue."
  (interactive)
  (lyrion--player-command
   (lambda (status)
     (let ((mode (or (lyrion--number (lyrion--get status "playlist repeat")) 0)))
       (list "playlist" "repeat" (number-to-string (% (1+ mode) 3)))))))

(defun lyrion-crossfade ()
  "Turn smart crossfading on or off for the player shown.
On, tracks crossfade, except successive tracks from one album, which play
gaplessly. Off, they play with no transition. Any other transition, such
as a plain crossfade or a fade, counts as on, so it is turned off."
  (interactive)
  (let* ((buffer (get-buffer lyrion-buffer-name))
         (type (and buffer
                    (lyrion--get (buffer-local-value 'lyrion--transition buffer)
                                 "transitionType"))))
    (lyrion--player-command
     (lambda (_status)
       (unless type
         (user-error "The player's crossfade setting is not known yet"))
       (if (equal type "0")
           '("playerpref" "transitionType" "1")
         '("playerpref" "transitionType" "0"))))
    (if (equal type "0")
        (progn (lyrion--player-command '("playerpref" "transitionSmart" "1"))
               (message "Smart crossfade on"))
      (message "Crossfade off"))))

(defun lyrion-lyrics ()
  "Show the lyrics of the track playing beside the player, or hide them.
They come from LRCLIB, which is sent the track's artist, title, album and
length."
  (interactive)
  (let ((window (get-buffer-window lyrion-lyrics-buffer-name)))
    (if (window-live-p window)
        (quit-window nil window)
      (let ((buffer (get-buffer-create lyrion-lyrics-buffer-name))
            (player (get-buffer lyrion-buffer-name)))
        (with-current-buffer buffer
          (unless (derived-mode-p 'lyrion-lyrics-mode)
            (lyrion-lyrics-mode)))
        (lyrion--lyrics-load
         buffer (and player (lyrion--track (buffer-local-value 'lyrion--status
                                                                 player))))
        (let ((window (display-buffer buffer (lyrion--lyrics-display-action))))
          (when (and lyrion-lyrics-select-window (window-live-p window))
            (select-window window)))))))

(defun lyrion-quit ()
  "Hide the lyrics, if they are showing, and quit the player's window.
Playback goes on: the server is what plays."
  (interactive)
  (when-let* ((window (get-buffer-window lyrion-lyrics-buffer-name)))
    (quit-window nil window))
  (quit-window))

(defconst lyrion--sleep-choices
  '(("15 minutes" . 15) ("30 minutes" . 30) ("45 minutes" . 45)
    ("60 minutes" . 60) ("90 minutes" . 90) ("Off" . 0))
  "The sleep timer's choices, as (NAME . MINUTES); none is off.")

(defun lyrion-sleep ()
  "Have the player stop after a while, or cancel that.
In the last minute the server fades the music out, then turns the player
off. The player line shows when it will sleep."
  (interactive)
  (let* ((status (buffer-local-value 'lyrion--status (lyrion--player-buffer)))
         (names (mapcar #'car lyrion--sleep-choices))
         (time (lyrion--sleep-time status (buffer-local-value
                                           'lyrion--status-time
                                           (lyrion--player-buffer))))
         (choice (completing-read
                  (if time (format "Sleep in (now at %s): " time) "Sleep in: ")
                  (lambda (string predicate action)
                    (if (eq action 'metadata)
                        '(metadata (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action names string predicate)))
                  nil t))
         (minutes (cdr (assoc choice lyrion--sleep-choices))))
    (unless minutes
      (user-error "Nothing chosen"))
    (lyrion--player-command (list "sleep" (number-to-string (* 60 minutes))))
    (message (if (zerop minutes)
                 "Sleep timer off"
               (format "Sleeping in %d minutes" minutes)))))

(defun lyrion-copy-track ()
  "Copy the track playing as \"Artist - Release - Title\", for pasting.
An artist or release the server does not know is left out."
  (interactive)
  (let* ((status (buffer-local-value 'lyrion--status (lyrion--player-buffer)))
         (title (or (lyrion--get status "title")
                    (lyrion--get status "current_title")
                    (user-error "Nothing is playing")))
         (text (string-join (delq nil (list (lyrion--get status "artist")
                                            (lyrion--get status "album")
                                            title))
                            " - ")))
    (kill-new text)
    (message "Copied %s" text)))

(defun lyrion-lyrics-copy ()
  "Copy the lyrics showing, without their times, for pasting elsewhere."
  (interactive)
  (let* ((buffer (get-buffer lyrion-lyrics-buffer-name))
         ;; Hidden lyrics do not follow the track, so they may be stale.
         (text (and buffer
                    (get-buffer-window buffer t)
                    (lyrion--lyrics-text
                     (buffer-local-value 'lyrion--lyrics-all buffer)))))
    (unless text
      (user-error "No lyrics are showing"))
    (kill-new text)
    (message "Copied the lyrics, %d lines"
             (length (split-string text "\n" t)))))

(defun lyrion-search-tracks ()
  "Choose a track from the library and play it alone."
  (interactive)
  (lyrion--search '(track) "Track"))

(defun lyrion-search-releases ()
  "Choose a release from the library and play it."
  (interactive)
  (lyrion--search '(release) "Release"))

(defun lyrion-search-album-artists ()
  "Choose an album artist from the library and play everything by them."
  (interactive)
  (lyrion--search '(artist) "Album artist"))

(defun lyrion-search-genres ()
  "Choose a genre from the library and play everything in it."
  (interactive)
  (lyrion--search '(genre) "Genre"))

(defun lyrion-search-works ()
  "Choose a recording of a work from the library and play it.
Its composer, release and album artist match too."
  (interactive)
  (lyrion--search '(work) "Work"))

(defun lyrion-search-playlists ()
  "Choose a playlist saved in the library and play it."
  (interactive)
  (lyrion--search '(playlist) "Playlist"))

(defun lyrion-search-all ()
  "Choose an item of any category chosen in the search menu, and play it.
The categories are album artists, genres, playlists, releases, works and
tracks, all of them unless some are switched off."
  (interactive)
  (lyrion--search (lyrion--chosen-kinds nil) "Search"))

(defun lyrion-search-qobuz ()
  "Search Qobuz and play what is chosen.
It searches the categories chosen in the search menu that Qobuz has:
artists, playlists, releases and tracks. Each comes in Qobuz's order, most
relevant first. The keys are as for the library: an artist plays their
songs, those Qobuz finds for their name, up to 200, and
\\<lyrion-search-minibuffer-map>\\[lyrion-search-open-exit]
opens the artist into their releases instead."
  (interactive)
  (let ((kinds (lyrion--chosen-kinds t))
        (term (string-trim (read-string "Search Qobuz: "))))
    (when (string-empty-p term)
      (user-error "Nothing to search for"))
    (message "Searching Qobuz for %s..." term)
    (let ((results (seq-filter #'cdr (lyrion--qobuz-search term kinds)))
          (lyrion--shuffle-chosen nil)
          (lyrion--open-chosen nil))
      (message nil)
      (unless results
        (user-error "Nothing on Qobuz matches %s" term))
      (pcase-let ((`(,kind . ,item)
                   (lyrion--read-item
                    (mapcar #'car results) "Qobuz"
                    (mapcar (lambda (result)
                              (lyrion--candidates-of (car result) (cdr result)))
                            results))))
        (when (and lyrion--open-chosen (eq kind 'qobuz-artist))
          (pcase-setq `(,kind . ,item)
                      (lyrion--read-release-by
                       (lyrion--item-name kind item) 'qobuz-release
                       (lyrion--qobuz-artist-releases item))))
        (lyrion--play-item kind item lyrion--shuffle-chosen)))))

(transient-define-prefix lyrion-search-menu ()
  "Choose what to search the library for.
The switches choose the categories that Everything and Qobuz include;
\\`C-x s' keeps them for this session and \\`C-x C-s' saves them for later
ones. The playback keys, but for the volume, whose `-' would clash with the
switches, work here too and keep the menu open, so the music can be
changed while choosing."
  :value '("artists" "genres" "playlists" "releases" "works"
           "tracks")
  ["Everything (/) and Qobuz (q) include  (C-x C-s saves the choice)"
   :class transient-row
   ;; Each shows as its key and name, lit when on.
   ("-a" "Artists" "artists" :format " %k %v")
   ("-g" "Genres" "genres" :format " %k %v")
   ("-p" "Playlists" "playlists" :format " %k %v")
   ("-r" "Releases" "releases" :format " %k %v")
   ("-w" "Works" "works" :format " %k %v")
   ("-t" "Tracks" "tracks" :format " %k %v")]
  [["Search the library"
    ("t" "Tracks" lyrion-search-tracks)
    ("r" "Releases" lyrion-search-releases)
    ("a" "Album artists" lyrion-search-album-artists)
    ("g" "Genres" lyrion-search-genres)
    ("w" "Works" lyrion-search-works)
    ("l" "Playlists" lyrion-search-playlists)
    ("/" "Everything" lyrion-search-all)
    ("q" "Qobuz" lyrion-search-qobuz :if lyrion--qobuz-p)]
   ["Radio"
    ("p" "Radio Paradise" lyrion-radio-paradise :if lyrion--radio-paradise-p)]
   ["Playback"
    ("SPC" "Play or pause" lyrion-play-pause :transient t)
    ("<" "Previous track" lyrion-previous :transient t)
    (">" "Next track" lyrion-next :transient t)
    ("<left>" lyrion-skip-back :transient t
     :description (lambda () (format "Skip back %s s" lyrion-skip-seconds)))
    ("<right>" lyrion-skip-forward :transient t
     :description
     (lambda () (format "Skip forward %s s" lyrion-skip-seconds)))
    ("L" "Show or hide the lyrics" lyrion-lyrics :transient t)]])

(defun lyrion-search ()
  "Search the library and play what is chosen in place of the queue.
A menu asks what to search for. The library is listed in the background as
it opens, once a session, so it is usually ready by the next key."
  (interactive)
  ;; The plugins first: the server answers one request at a time, and
  ;; behind the library's listings their answer would come too late.
  (lyrion--check-plugins)
  (lyrion--library-prefetch)
  (transient-setup 'lyrion-search-menu))

(defun lyrion--queue (id)
  "Return the queue of the player ID as (CURRENT . TRACKS), in play order.
CURRENT is the position of the track playing, and TRACKS are alists, each
with its position as `playlist index'. Positions count from 0."
  (let ((reply (lyrion--query-wait (list id "status" "0" "100000" "tags:al")
                                   "Could not fetch the queue from %s")))
    (cons (lyrion--number (lyrion--get (lyrion--fields reply)
                                       "playlist_cur_index"))
          (lyrion--group reply "playlist index"))))

(declare-function vertico--goto "vertico")
(declare-function vertico--exhibit "vertico")

(defun lyrion--select-on-open (index)
  "Have Vertico, when it is in use, start with candidate INDEX selected.
Vertico draws the candidates once the minibuffer opens, and the selection
moves just after. Vertico has no public way to do this, so its internal
`vertico--goto' is used, when there; other completion interfaces start at
the first candidate."
  (when (and (bound-and-true-p vertico-mode)
             (fboundp 'vertico--goto)
             (fboundp 'vertico--exhibit))
    (letrec ((select (lambda ()
                       (remove-hook 'post-command-hook select t)
                       (vertico--goto index)
                       (vertico--exhibit))))
      ;; After Vertico's own drawing, which runs at the default depth.
      (add-hook 'post-command-hook select 90 t))))

(defun lyrion-queue ()
  "Show the player's queue in play order, and play the track chosen.
The track playing is marked, and with Vertico the selection starts on it,
so the tracks before and after it are a move away.
The queue is asked for afresh each time. Each track shows its title,
artist and album, all of which match; a track queued twice has a number to
tell the two apart. Choosing a track plays it where it is, the queue
staying as it was; choosing the one playing starts it again."
  (interactive)
  (let* ((player (buffer-local-value 'lyrion--player (lyrion--player-buffer)))
         (id (or (lyrion--get player "playerid")
                 (user-error "No Lyrion player is shown")))
         (queue (lyrion--queue id))
         (current (car queue))
         (positions (make-hash-table :test #'equal))
         (texts
          (mapcar (lambda (track)
                    (let* ((position (lyrion--number
                                      (lyrion--get track "playlist index")))
                           (base (lyrion--with-detail
                                  (or (lyrion--get track "title") "")
                                  (list (lyrion--get track "artist")
                                        (lyrion--get track "album"))))
                           (text base)
                           (copy 1))
                      ;; Positions are left out, so as not to be taken for
                      ;; the track's number on its release; a repeat gets a
                      ;; number of its own instead.
                      (while (gethash (substring-no-properties text) positions)
                        (setq copy (1+ copy)
                              text (format "%s (%d)" base copy)))
                      (puthash (substring-no-properties text) position
                               positions)
                      text))
                  (cdr queue)))
         ;; Where the track playing is among them, to select it.
         (start (and current
                     (seq-position texts current
                                   (lambda (text position)
                                     (eql (gethash (substring-no-properties
                                                    text)
                                                   positions)
                                          position))))))
    (unless texts
      (user-error "The queue is empty"))
    (let* ((affix (lambda (candidates)
                    (mapcar (lambda (candidate)
                              (list candidate
                                    (if (eql (gethash (substring-no-properties
                                                       candidate)
                                                      positions)
                                             current)
                                        "▶ "
                                      "  ")
                                    ""))
                            candidates)))
           ;; The queue's own order, whatever the completion UI would sort by.
           (table (lambda (string predicate action)
                    (if (eq action 'metadata)
                        `(metadata (category . lyrion-queue)
                                   (affixation-function . ,affix)
                                   (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action texts string predicate))))
           (choice (minibuffer-with-setup-hook
                       (:append (lambda ()
                                  (when start (lyrion--select-on-open start))))
                     (completing-read
                      (if current
                          (format "Queue (playing %d of %d): "
                                  (1+ current) (length texts))
                        "Queue: ")
                      table nil t)))
           (position (gethash (substring-no-properties choice) positions)))
      (unless position
        (user-error "Nothing chosen"))
      ;; The track playing too, which then starts again.
      (lyrion--player-command
       (list "playlist" "index" (number-to-string position))))))

(defun lyrion--shown (variable)
  "Return VARIABLE's value in the player's buffer, or nil without one."
  (when-let* ((buffer (get-buffer lyrion-buffer-name)))
    (buffer-local-value variable buffer)))

(defun lyrion--shuffle-description ()
  "Describe the shuffle key with whether the player shuffles."
  (format "Shuffle (%s)"
          (if (member (lyrion--get (lyrion--shown 'lyrion--status)
                                   "playlist shuffle")
                      '("1" "2"))
              "on"
            "off")))

(defun lyrion--repeat-description ()
  "Describe the repeat key with the player's repeat mode."
  (format "Repeat (%s)"
          (pcase (lyrion--get (lyrion--shown 'lyrion--status)
                              "playlist repeat")
            ("1" "track")
            ("2" "all")
            (_ "off"))))

(defun lyrion--sleep-description ()
  "Describe the sleep key with when the player will sleep, if it will."
  (if-let* ((time (lyrion--sleep-time (lyrion--shown 'lyrion--status)
                                      (lyrion--shown 'lyrion--status-time))))
      (format "Sleep timer (at %s)" time)
    "Sleep timer"))

(defun lyrion--crossfade-description ()
  "Describe the crossfade key with the player's crossfade setting."
  (let ((name (lyrion--transition-name (lyrion--shown 'lyrion--transition))))
    (if name
        (format "Crossfade (%s)" (if (equal name "smart crossfade")
                                     "smart"
                                   name))
      "Crossfade")))

(transient-define-prefix lyrion-dispatch ()
  "Show every key of the player and the lyrics, and run the one pressed.
The labels give the player's repeat mode and crossfade setting. Moving
through the queue and the track keeps the menu open; other keys close it."
  [["Playback"
    ("SPC" "Play or pause" lyrion-play-pause :transient t)
    ("<" "Previous track" lyrion-previous :transient t)
    (">" "Next track" lyrion-next :transient t)
    ("<left>" lyrion-skip-back :transient t
     :description (lambda () (format "Skip back %s s" lyrion-skip-seconds)))
    ("<right>" lyrion-skip-forward :transient t
     :description
     (lambda () (format "Skip forward %s s" lyrion-skip-seconds)))
    ("+" "Volume up (or =)" lyrion-volume-up :transient t)
    ("-" "Volume down" lyrion-volume-down :transient t)
    ;; `+' without shift: bound, but drawn as nothing, last in the column.
    ("=" lyrion-volume-up :transient t :format "")]
   ["Queue"
    ("S" lyrion-shuffle-toggle :description lyrion--shuffle-description)
    ("S-<return>" "Shuffle afresh" lyrion-shuffle)
    ("O" "Play in order, from the top" lyrion-play-in-order)
    ("R" lyrion-repeat :description lyrion--repeat-description)
    ("X" lyrion-crossfade :description lyrion--crossfade-description)
    ("I" "Inspect the queue" lyrion-queue)]
   ["Library"
    ("/" "Search..." lyrion-search)]]
  ;; A second row, so the menu fits a frame of about 80 columns.
  [["Player"
    ("p" "Choose the player" lyrion-select-player)
    ("w" "Copy artist, release and title" lyrion-copy-track)
    ("Z" lyrion-sleep :description lyrion--sleep-description)
    ("g" revert-buffer
     :description
     (lambda () (if (derived-mode-p 'lyrion-lyrics-mode)
                    "Fetch the lyrics again"
                  "Reconnect")))]
   ["Lyrics"
    ("L" "Show or hide" lyrion-lyrics)
    ("C" "Copy" lyrion-lyrics-copy)]])

(provide 'lyrion)

;;; lyrion.el ends here
