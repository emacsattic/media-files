;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;; * open file on line
;; * bind 1, 2, 3, etc. to toggle users on line
;; * highlight background when mouse hover
;; * navigate to previous/next checkbox/filename with f/b
;; * toggle whether to display watched files
;; * toggle all checkboxes on line
;; * save/load database file
;; * when open, watch database for changes and automatically re-load;
;;   otherwise, keep database closed when not in use.
;; * detect duplicates, only filename (not directory) determines uniqueness;
;;   handle case where file is moved.
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
(unless media-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'media-files-update)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (setq media-files-mode-map map)))

(defvar media-files-checkbox-map nil)
(setq media-files-checkbox-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-1)] 'media-file-toggle-checkbox)
    map))

(defun media-files-mode ()
  (kill-all-local-variables)
  (use-local-map media-files-mode-map)
  (use-local-map media-files-mode-map)
  (setq major-mode 'media-files-mode
        mode-name "Media-Files")
  (set (make-local-variable 'revert-buffer-function) #'media-files-update)
  (run-mode-hooks 'media-files-mode-hook)
  )

(defun media-files-update (arg &optional silent)
  ;; the dummy arg is needed so we're compatible with revert-buffer-function
  (interactive "P")
  (media-files-assert-mode)
  (let ((line (line-number-at-pos (point))))
    (unless silent
      (message "Updating media list..."))
    (toggle-read-only 0)
    (erase-buffer)
    (mapc 'media-file-insert-line *media-files*)
    (toggle-read-only 1)
    (goto-char (point-min))
    (forward-line (1- line))
    (unless silent
      (message "Updating media list...done"))
    ))

(defun media-file-insert-line (media-file &optional no-newline)
  (media-files-assert-mode)
  (beginning-of-line)
  (let ((beg-line (point))
        beg-file-name)
    (dolist (user media-users)
      (let ((beg (point))
            (check-mark (if (memq user (media-file-users-watched media-file))
                            "X" " ")))
        (insert (format "%s [%s]" user check-mark))
        (add-text-properties beg (point)
            (list 'mouse-face 'hilight
                  'keymap media-files-checkbox-map
                  'help-echo (format "Left click: toggle user %s" user)
                  'user user))
        (insert "     ")))
    (setq beg-file-name (point))
    (insert (file-name-nondirectory (media-file-path media-file)))
    (add-text-properties beg-line (point)
            (list 'media-file media-file))
    (add-text-properties beg-file-name (point)
            (list 'help-echo "Left click: open file")))
  (unless no-newline (newline)))

(defun media-file-toggle-checkbox (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (let ((media-file (get-text-property (point) 'media-file))
          (user (get-text-property (point) 'user))
          beg)
      ;; note that because *media-files* is a defstruct, this line actually
      ;; modifies the global variable, not some copy of it
      (media-file-toggle-user-watched media-file user)
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (delete-region beg (point))
      (media-file-insert-line media-file 'no-newline)
      )))

(defun display-media-files ()
  (interactive)
  (with-current-buffer (get-buffer-create media-file-buffer)
    (when (not (eq major-mode 'media-files-mode))
      (media-files-mode))
    (media-files-update t))

  ;; TODO customize this behavior, support save and restore window config
  ;;(display-buffer media-file-buffer)
  (switch-to-buffer-other-window media-file-buffer)
  )

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

(defun media-file-toggle-user-watched (media-file user)
  (if (media-user-watched-p media-file user)
      (setf (media-file-users-watched media-file)
            (delete user (media-file-users-watched media-file)))
    (push user (media-file-users-watched media-file))))

(defun media-file-users-not-watched (media-file &optional users)
  "Return the subset of USERS who have not watched MEDIA-FILE.
If nil, USERS defaults to `media-users'."
  (unless users (setq users media-users))
  (set-difference users (media-file-users-watched media-file)))

(defun media-files-get-unwatched (&optional media-files users)
  "Return all MEDIA-FILES that are not watched by at least one of
USERS.  If nil, MEDIA-FILES defaults to `*media-files*' and USERS
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
