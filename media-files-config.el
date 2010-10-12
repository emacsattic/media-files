(require 'media-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my preferred config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar phil-shows
  '("Big Love"
    "Cleveland Show"
    "Bored to Death"
    "Star Wars Clone Wars"
    "Family Guy"
    "Colbert Report"
    "Daily Show"
    "Being Erica"
    "Breaking Bad"
    "Big Love"
    "Californication"
    "Caprica"
    "Dexter"
    "Futurama"
    "Mad Men"
    "Simpsons"
    "South Park"
    "Weeds"

    "Angel"
    "Bones"
    "Dollhouse"
    "Battlestar Galactica"
    "Carnivale"
    "Curb Your Enthusiasm"
    "Deadwood"
    "Extras"
    "Firefly"
    "John Adams"
    "Malcolm in the Middle"
    "Rome"
    "Scrubs"
    "Sex and the City"
    "Star Trek Voyager"
    "Star Trek DS9" ;; or Deep Space Nine
    "The 4400"
    "The Shield"
    "The Wire"
    "The Wonder Years"
    "Veronica Mars"
    "X-Files"
    "Star Trek The Next Generation") ;; or TNG
  )

(defun media-files-setup ()
  (setq media-files-series-alist phil-shows)

  (setq media-dir-prefix
        (cond ((memq system-type '(windows-nt cygwin)) "z:/")
              ((eq system-type 'darwin) "/Volumes/share/")
              (t "~")))

  (setq media-dir '("shows/" "torrents/"))

  (setq media-files-command-path
        (cond ((memq system-type '(windows-nt cygwin))
               "c:/Program Files/VideoLAN/VLC/vlc")
              ((eq system-type 'darwin)
               "open")
              (t "vlc")))

  (setq media-users '(erinne philip))
  (setq media-files-sort-by 'time)
  )

