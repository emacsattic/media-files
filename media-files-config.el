(require 'media-files)
(require 'episode-names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my preferred config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar phil/series-names
  '("Big Love" "Cleveland Show" "Bored to Death" "Star Wars Clone Wars" "Family Guy" "Colbert Report" "Daily Show" "Being Erica" "Breaking Bad" "Big Love" "Caprica" "Dexter" "Futurama" "Mad Men" "Simpsons" "South Park" "Weeds" "Dollhouse" "Carnivale" "Curb Your Enthusiasm" "Deadwood" "John Adams" "Malcolm in the Middle" "Star Trek Voyager" "Star Trek DS9" "Star Trek The Next Generation" "The 4400" "The Shield" "The Wire" "The Wonder Years" "Veronica Mars" "Sex and the City" "X-Files" "Bones" "Extras" "Firefly" "Scrubs" "Rome" "Battlestar Galactica" "Californication" "Angel"))

(defun media-files-setup ()
  (setq series-list
        (append
         (list
          (make-series :name "Daily Show"
                       :episode-date-regexp '("\\([0-9][0-9][0-9][0-9]\\)\\.\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)" 1 2 3))
          (make-series :name "Colbert Report"
                       :episode-date-regexp '("\\([0-9][0-9][0-9][0-9]\\)\\.\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)" 1 2 3))
          (make-series :name "Firefly"
                       :episode-number-regexp '("/\\([0-9][0-9]\\)" 1))
          (make-series :name "John Adams"
                       :episode-number-regexp '("part\\.?\\([0-9]\\)" 1))
          )
         (mapcar (lambda (x) (make-series :name x)) phil/series-names)))

  (setq media-dir-prefix
        (cond ((memq system-type '(windows-nt cygwin)) "z:/")
              ((eq system-type 'darwin) "/Volumes/share/")
              (t "/mnt/share/")))

  (setq media-dir '("shows/" "torrents/"))

  (setq media-files-command-path
        (cond ((memq system-type '(windows-nt cygwin))
               "c:/Program Files/VideoLAN/VLC/vlc")
              ((eq system-type 'darwin)
               "open")
              (t '("vlc" "mplayer" "totem"))))

  (setq media-users '(erinne philip))
  (setq media-files-sort-by 'timestamp)
  )

(provide 'media-files-config)
