(require 'media-files)
(require 'episode-names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my preferred config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO The 4400 needs a special regexp. "The 4400.408" is identified as season
;; 44, episode 0

;; TODO support for two-part episodes (such as X-Files season 19, episode 19-20)

(setq series-list
  (append

   ;; shows with custom regexp
   (list (make-series :name "Firefly"
                      :episode-number-regexp '("/\\([0-9][0-9]\\)" 1))
         (make-series :name "John Adams"
                      :episode-number-regexp '("part\\.?\\([0-9]\\)" 1))
         (make-series :name "Star Wars Clone Wars"
                      :name-regexp "star.wars.\\(the.\\)?clone.wars"))

   ;; shows that use a date
   (mapcar (lambda (x) (make-series :name x :episode-date-regexp t))
           '("Daily Show" "Colbert Report" "Conan"))

   ;; the rest of the shows
   (mapcar (lambda (x) (make-series :name x))
           '("Big Love" "Cleveland Show" "Bored to Death"  "Family Guy" "Colbert Report" "Daily Show" "Being Erica" "Breaking Bad" "Big Love" "Caprica" "Dexter" "Futurama" "Mad Men" "South Park" "Weeds" "Dollhouse" "Carnivale" "Curb Your Enthusiasm" "Deadwood" "Malcolm in the Middle" "Star Trek Voyager" "Star Trek DS9" "Star Trek The Next Generation" "The 4400" "The Shield" "Rubicon" "The Wire" "Sister Wives" "The Walking Dead" "The Wonder Years" "Veronica Mars" "Sex and the City" "X-Files" "Simpsons" "Bones" "Extras" "Stargate Universe" "Scrubs" "Rome" "Battlestar Galactica" "Californication" "Angel" "Conan"))))

(setq media-dir-prefix
      (cond ((memq system-type '(windows-nt cygwin)) "z:/")
            ((eq system-type 'darwin) "/Volumes/share/")
            (t "/mnt/share/")))

(setq media-dir '("shows/" "torrents/"))

(setq media-files-command-path
      (cond ((memq system-type '(windows-nt cygwin))
             "c:/Program Files/VideoLAN/VLC/vlc.exe")
            ((eq system-type 'darwin)
             "open")
            (t '("vlc" "mplayer" "totem"))))

(setq media-users '(erinne philip))
(setq media-files-sort-by 'timestamp)

(provide 'media-files-config)
