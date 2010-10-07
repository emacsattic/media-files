;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;; * save/load database file
;; * when open, watch database for changes and automatically re-load;
;;   otherwise, keep database closed when not in use.
;; * toggle check-box when <return> or mouse click
;; * navigate with n/p/f/b
;; * 'w' to toggle whether to display watched files
;; * detect duplicates, only filename (not directory) determines uniqueness;
;;   handle case where file is moved.
;; * click on file to watch it
;; * metadata - recognize show and episode names, figure out where to refile

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables

(defvar media-dir-prefix
  (cond ((eq system-type 'windows-nt) "z:/")
        ((eq system-type 'darwin) "/Volumes/share")

        ;; assume we're on the file server, where everything is local
        (t "~")))

(defvar media-dir '("shows/" "torrents/")
  "A directory or list of directories - relative to
`media-dir-prefix' where media-files are found.")

(defvar media-file-regexp "\\(\\.avi$\\|\\.mp4$\\|\\.mpg$\\|\\.mpeg\\)")

(defvar media-users '(erinne philip))

(defstruct media-file path users-watched)

(defvar *media-files* nil)

(defconst media-file-buffer " *media-files*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defvar media-files-mode-map nil)

(defun media-files-setup-map ()
  (let ((map (make-sparse-keymap)))
    (setq media-files-mode-map map)))

(defun media-files-mode ()
  (kill-all-local-variables)
  (media-files-setup-map)
  (use-local-map media-files-mode-map)
  (setq major-mode 'media-files-mode
        mode-name "Media-Files")
  ;; (set (make-local-variable 'revert-buffer-function) #'media-files-update)
  ;; (run-mode-hooks 'media-files-mode-hook)
  )

(defun media-files-update (&optional silent)
  (interactive "P")
  (media-files-assert-mode)
  (let ((line (line-number-at-pos (point))))
    (unless silent
      (message "Updating media list..."))
    (toggle-read-only 0)
    (erase-buffer)
    (dolist (x *media-files*)
      (dolist (user media-users)
        (let ((check-mark (if (memq user (media-file-users-watched x))
                              "X" " ")))
          (insert (format "%s [%s]     " user check-mark))))
      (insert (file-name-nondirectory (media-file-path x)))
      (newline))
    (toggle-read-only 1)
    (goto-char (point-min))
    (forward-line (1- line))
    (unless silent
      (message "Updating media list...done"))
    ))

(defun display-media-files ()
  (interactive)
  (with-current-buffer (get-buffer-create media-file-buffer)
    (when (not (eq major-mode 'media-files-mode))
      (media-files-mode))
    (media-files-update t))

  (display-buffer media-file-buffer))

(defun initialize-media-files ()
  (setq *media-files* (scan-media-files)))

(defun scan-media-files (&optional dir)
  (unless dir (setq dir media-dir))
  (if (listp dir)
      (apply 'append (mapcar 'scan-media-files dir))
    (let ((full-dir (concat media-dir-prefix dir))
          contents subdirs files)
      (setq contents
            (directory-files-and-attributes full-dir t "^[^.]" 'nosort))
      (dolist (x contents)
        (let ((file (dired-make-relative (car x) media-dir-prefix))
              (is-dir (car (cdr x))))
          (cond (is-dir (push file subdirs))
                ((string-match media-file-regexp file)
                 (push (make-media-file :path file) files)))))
      (setq files (apply 'append files (mapcar 'scan-media-files subdirs)))
      files)))

(defun media-user-watched-p (media-file user)
  (memq user (media-file-users-watched media-file)))

(defun media-file-users-not-watched (media-file &optional users)
  "Return the subset of USERS who have not watched MEDIA.
If nil, USERS defaults to `media-users'."
  (unless users (setq users media-users))
  (set-difference users (media-file-users-watched media-file)))

(defun media-files-get-unwatched (&optional media-files users)
  "Return all MEDIA-FILES that are not watched by at least one of
USERS.  If nil, MEDIA-FILES defaults to `*media-files' and USERS
defaults to `media-users'."
  (unless media-files (setq media-files *media-files*))
  ;; remove all media files that have been watched by everyone in USERS
  (remove-if (lambda (x) (null (media-file-users-not-watched x users))) media-files))

(defsubst media-files-assert-mode ()
  (assert (derived-mode-p 'media-files-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some testing and debugging

(ignore
 '(
   (setq foo (make-media-file :path "/" :users-watched '(philip)))
   (initialize-media-files)

   (length (media-files-get-unwatched))
   (media-files-get-unwatched *media-files*)
 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'media-files)
