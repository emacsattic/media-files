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
;; * support undo
;; * save/load database file
;; * toggle all checkboxes on line
;; * support filters
;; * keep original media file list separate from the one in buffer;
;;   this lets us, for example, remove a filter or sort.
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

(defvar media-dir-prefix "~")

(defvar media-dir "videos"
  "A directory or list of directories - relative to
`media-dir-prefix' - where media-files are found.")

;; what external command to execute to open media files
(defvar media-files-command-path "vlc"
  "What program to use to open media files.")

(defvar media-file-regexp "\\(\\.avi$\\|\\.mp4$\\|\\.mpg$\\|\\.mpeg\\)")

(defvar media-users '(me))

(defstruct media-file path time users-watched series-name episode-number)

(defvar *media-files* nil)

(defconst media-file-buffer " *media-files*")

(defvar media-files-filter-watched nil
  "If non-nil, then media files that have been watched by all
users in `media-users' will not be displayed in the media file
list.")

(defvar media-files-sort-by nil
  "How to sort the list of media files.  Can be nil, `name', or
`time'.")

;; TODO document this.  see media-files-config.el for example
(defvar media-files-series-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions related to the media file display list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst media-files-assert-mode ()
  (assert (derived-mode-p 'media-files-mode)))

(defvar media-files-mode-map nil)
(unless media-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'media-file-next-item)
    (define-key map (kbd "<backtab>") 'media-file-previous-item)
    (define-key map (kbd "f") 'media-file-next-item)
    (define-key map (kbd "b") 'media-file-previous-item)
    (define-key map (kbd "g") 'media-files-update)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "o") 'media-file-open-file)
    (define-key map (kbd "t") 'media-files-toggle-watched)
    (define-key map (kbd "1")
      (lambda () (interactive) (media-file-toggle-checkbox 0)))
    (define-key map (kbd "2")
      (lambda () (interactive) (media-file-toggle-checkbox 1)))
    (define-key map (kbd "3")
      (lambda () (interactive) (media-file-toggle-checkbox 2)))
    (define-key map (kbd "4")
      (lambda () (interactive) (media-file-toggle-checkbox 3)))
    (setq media-files-mode-map map)))

(defvar media-files-checkbox-map nil)
(unless media-files-checkbox-map
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
    (dolist (media-file *media-files*)
      (when (or (not media-files-filter-watched)
            (media-file-users-not-watched media-file))
        (media-file-insert-line media-file)))
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
  (assert (not buffer-read-only))
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
        (insert "   ")
        (put-text-property beg (point) 'user user)))
    (insert (format-time-string "%D" (media-file-time media-file)) "  ")
    (setq beg-file-name (point))
    (insert (or (media-file-series-name media-file) "unknown"))
    (insert " ")
    (insert (format "%s" (media-file-episode-number media-file)))
    (add-text-properties beg-line (point)
            (list 'media-file media-file))
    (add-text-properties beg-file-name (point)
            (list 'mouse-face 'highlight
                  'keymap media-files-filename-map
                  'help-echo (media-file-base-name media-file))))
                  ;; 'help-echo "Middle click: open file"
  (insert " ") ;; keeps the mouse highlight from spilling over to next line
  (unless no-newline (newline)))

(defun media-file-mouse-toggle-checkbox (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (media-file-toggle-checkbox)))

(defun media-file-toggle-checkbox (&optional arg)
  (interactive)
  (media-files-assert-mode)
  (let ((media-file (get-text-property (point) 'media-file))
        (user (media-file-nth-user arg)))
    ;; note that because *media-files* is a defstruct, this line actually
    ;; modifies the global variable, not some copy of it
    (when user
      (media-file-toggle-user-watched media-file user)
      (media-file-update-line media-file))
    ))

(defun media-file-update-line (media-file)
  (let ((saved-point (point))
        beg)
    (forward-line 0)
    (setq beg (point))
    (end-of-line)
    (toggle-read-only 0)
    (delete-region beg (point))
    (media-file-insert-line media-file 'no-newline)
    (toggle-read-only 1)
    (goto-char saved-point)))

(defun media-file-nth-user (&optional arg)
  "Return the ARG-th user on the current line.  If ARG is nil,
then return the user under point.  Note that if ARG is greater
than the number of user on the current line, then this function
will wrap around to the next line, counting the filenames on each
line as an item."
  (media-files-assert-mode)
  (let (user saved-point)
    (when arg
      (setq saved-point (point))
      (forward-line 0)
      (media-file-next-item (prefix-numeric-value arg)))
    (setq user (get-text-property (point) 'user))
    (when arg
      (goto-char saved-point))
    user))

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
    (open-media-file media-file)))

(defun media-files-toggle-watched ()
  (interactive)
  (setq media-files-filter-watched (not media-files-filter-watched))
  (when (derived-mode-p 'media-files-mode)
    (media-files-update t)))

(defun display-media-files ()
  (interactive)
  (sort-media-files)
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
  "Use `scan-media-files' to initialize `*media-files*'. This is
expensive!"
  (setq *media-files* (scan-media-files)))

(defun scan-media-files (&optional dir)
  "Scan DIR recursively for all media files that match
`media-file-regexp'.  The result is a list of `media-file'
structures.  DIR can be a list, in which case each directory in
DIR is scanned and the results are accumulated into a single
list.  This is an expensive operation and it can take several
seconds depending on the size of the directories."
  (unless dir (setq dir media-dir))
  (if (listp dir)
      (apply 'append (mapcar 'scan-media-files dir))
    (let ((full-dir (concat media-dir-prefix dir))
          contents subdirs files)
      (setq contents
            (directory-files-and-attributes full-dir t "^[^.]" 'nosort))
      (dolist (x contents)
        (let ((file (file-relative-name (car x) media-dir-prefix))
              (name (file-name-nondirectory (car x)))
              (is-dir (car (cdr x))))
          (cond (is-dir (push file subdirs))
                ((string-match media-file-regexp file)
                 (let ((media-file
                        (make-media-file :path file :time (nth 6 x))))
                   (media-file-guess-series-name media-file)
                   (media-file-guess-episode-number media-file)
                   (push media-file files))))))
      (setq files (apply 'append files (mapcar 'scan-media-files subdirs)))
      files)))

(defun sort-media-files (&optional sort-by)
  "Sort `*media-files*' according to SORT-BY.  If nil, SORT-BY
defaults to `media-files-sort-by'."
  (when (null sort-by) (setq sort-by media-files-sort-by))
  (cond ((equal sort-by 'name)
         (setq *media-files* (sort *media-files* 'media-file-name-lessp)))
        ((equal sort-by 'time)
         (setq *media-files* (sort *media-files* 'media-file-time-lessp)))))

(defun open-media-file (media-file)
  "Open MEDIA-FILE using the program specified by
`media-files-command-path'.  When called interactively, user can
select MEDIA-FILE from the list `*media-files*'."
  (interactive
   (let ((file-names (mapcar 'media-file-base-name *media-files*))
         file-name
         result)
     (setq file-name (completing-read "Open file: " file-names))
     (setq result (find-if (lambda (x) (string= (media-file-base-name x) file-name)) *media-files*))
     (list result)))
  (when media-file
    (call-process media-files-command-path nil 0 nil
                  (media-file-full-path media-file))))

(defun media-file-base-name (media-file)
  "Get the non-directory basename of the file in MEDIA-FILE."
  (file-name-nondirectory (media-file-path media-file)))

(defun media-file-full-path (media-file)
  "Get the full path to the file in MEDIA-FILE, including the
prefix specified by `media-dir-prefix'."
  (concat media-dir-prefix (media-file-path media-file)))

(defun media-user-watched-p (media-file user)
  "Return t if USER has watched MEDIA-FILE."
  (memq user (media-file-users-watched media-file)))

(defun media-file-toggle-user-watched (media-file user)
  "Toggle whether USER has watched MEDIA-FILE.  This
destructively modifies MEDIA-FILE by side effect."
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

(defun media-file-name-lessp (a b)
  (string-lessp (media-file-base-name a) (media-file-base-name b)))

(defun media-file-time-lessp (a b)
  (< 0 (time-to-seconds (time-subtract (media-file-time a) (media-file-time b)))))
  ;; (time-less-p (media-file-time b) (media-file-time a)))

;; functions for getting the series names and episode numbers
;; TODO support per-show regexp for naming and episode numbering,
;;      such as shows that have season number in directory name

(defun make-series-regexp (show)
  (cond ((stringp show) (replace-regexp-in-string " " ".*" show))
        ((cdr show) (cdr show))
        (t (replace-regexp-in-string " " ".*" (car show)))))

(defun media-file-guess-series-name (media-file)
  (let ((result
         (find-if (lambda (x) (string-match (make-series-regexp x)
                                            (media-file-path media-file)))
                  media-files-series-alist)))
    (when result
      (setf (media-file-series-name media-file) result))))

(defun media-file-guess-episode-number (media-file)
  (let ((name (media-file-base-name media-file))
        (result))
    (setq result
          (if (or (string-match "S\\([0-9][0-9]?\\)E\\([0-9][0-9]?\\)" name)
                  (string-match "\\([0-9][0-9]?\\)x\\([0-9][0-9]?\\)" name)
                  (string-match "\\([0-9][0-9]?\\)\\([0-9][0-9]\\)" name)
                  (string-match "s\\([0-9][0-9]?\\)_ep\\([0-9][0-9]?\\)" name)
                  (string-match "season_\\([0-9][0-9]?\\)_ep_\\([0-9][0-9]?\\)" name))
              (cons (match-string 1 name) (match-string 2 name))))
    (when result
      (setf (media-file-episode-number media-file) result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'media-files)
