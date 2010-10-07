;; media-files.el
;;
;; Copyright (C) 2010 Philip Weaver
;; Author: Philip Weaver <philip.weaver@gmail.com>
;;
;; This file is not part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 2 as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;; * save/load database file
;; * bind 1, 2, 3, etc. to toggle users on line
;; * toggle whether to display watched files
;; * toggle all checkboxes on line
;; * custom display format (like ibuffer)
;; * when open, watch database for changes and automatically re-load;
;;   otherwise, keep database closed when not in use.
;; * detect duplicates, only filename (not directory) determines uniqueness;
;;   handle case where file is moved.
;; * metadata - recognize show and episode names, figure out where to refile

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar media-dir-prefix
  (cond ((memq system-type '(windows-nt cygwin)) "z:/")
        ((eq system-type 'darwin) "/Volumes/share")

        ;; assume we're on the file server, where everything is local
        (t "~")))

(defvar media-dir '("shows/" "torrents/")
  "A directory or list of directories - relative to
`media-dir-prefix' where media-files are found.")

;; what external command to execute to open media files
(defvar media-files-command-path
  (cond ((memq system-type '(windows-nt cygwin))
         "c:/Program Files/VideoLAN/VLC/vlc")
        ((eq system-type 'darwin)
         "open")
        (t "vlc")))

(defvar media-file-regexp "\\(\\.avi$\\|\\.mp4$\\|\\.mpg$\\|\\.mpeg\\)")

(defvar media-users '(erinne philip))

(defstruct media-file path users-watched)

(defvar *media-files* nil)

(defconst media-file-buffer " *media-files*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions related to the media file display list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar media-files-mode-map nil)
(unless media-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'media-file-next-item)
    (define-key map (kbd "b") 'media-file-previous-item)
    (define-key map (kbd "g") 'media-files-update)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "o") 'media-file-open-file)
    (setq media-files-mode-map map)))

(defvar media-files-checkbox-map nil)
(unless media-files-chekbox-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'media-file-toggle-checkbox)
    (define-key map [(mouse-1)] 'media-file-mouse-toggle-checkbox)
    (setq media-files-checkbox-map map)))

(defvar media-files-filename-map nil)
(unless media-files-filename-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-2)] 'media-file-mouse-open-file)
    (define-key map (kbd "RET") 'media-file-open-file)
    (setq media-files-filename-map map)))

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

;; hackish - relies on the fact that the 'user property changes at each item
(defun media-file-next-item (arg)
  (interactive "p")
  (if (< arg 0)
      (media-file-previous-item (- 0 arg))
    (dotimes (n arg)
      (let ((x (next-single-property-change (point) 'user)))
        (when x (goto-char x))))))

(defun media-file-previous-item (arg)
  (interactive "p")
  (if (< arg 0)
      (media-file-next-item (- 0 arg))
    (dotimes (n arg)
      (let ((x (previous-single-property-change (point) 'user)))
        (if x (goto-char x) (goto-char (point-min)))))))

(defun media-file-insert-line (media-file &optional no-newline)
  (media-files-assert-mode)
  (forward-line 0)
  (let ((beg-line (point))
        beg-file-name)
    (dolist (user media-users)
      (let ((beg (point))
            (check-mark (if (memq user (media-file-users-watched media-file))
                            "X" " ")))
        (insert (format "%s [%s]" user check-mark))
        (add-text-properties beg (point)
            (list 'mouse-face 'highlight
                  'keymap media-files-checkbox-map
                  'help-echo (format "Left click: toggle user %s" user)))
        (insert "     ")
        (put-text-property beg (point) 'user user)))
    (setq beg-file-name (point))
    (insert (file-name-nondirectory (media-file-path media-file)))
    (add-text-properties beg-line (point)
            (list 'media-file media-file))
    (add-text-properties beg-file-name (point)
            (list 'mouse-face 'highlight
                  'keymap media-files-filename-map
                  'help-echo "Middle click: open file")))
  (insert " ") ;; keeps the mouse highlight from spilling over to next line
  (unless no-newline (newline)))

(defun media-file-mouse-toggle-checkbox (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (media-file-toggle-checkbox)))

(defun media-file-toggle-checkbox ()
  (interactive)
  (media-files-assert-mode)
  (let ((media-file (get-text-property (point) 'media-file))
        (user (get-text-property (point) 'user))
        saved-point
        beg)
    (setq saved-point (point))
    ;; note that because *media-files* is a defstruct, this line actually
    ;; modifies the global variable, not some copy of it
    (media-file-toggle-user-watched media-file user)
    (forward-line 0)
    (setq beg (point))
    (end-of-line)
    (delete-region beg (point))
    (media-file-insert-line media-file 'no-newline)
    (goto-char saved-point)
    ))

(defun media-file-mouse-open-file (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (media-file-open-file)))

(defun media-file-open-file ()
  "Open the media file on the current line by executing
`media-files-command-path' on it."
  (interactive)
  (media-files-assert-mode)
  (let ((media-file (get-text-property (point) 'media-file)))
    (shell-command (concat "\"" media-files-command-path "\" \"" (media-file-full-path media-file) "\" &") nil nil)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions related to the media-file data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun media-file-full-path (media-file)
  (concat media-dir-prefix (media-file-path media-file)))

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

(provide 'media-files)
