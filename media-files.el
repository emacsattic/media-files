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
;; * custom display format (like ibuffer)
;; * when open, watch database for changes and automatically re-load;
;;   otherwise, keep database closed when not in use.
;; * determine where files should go based on metada (series and episode)

(require 'cl)
(require 'episode-names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar media-dir-prefix "~/"
  "Prefix directory where all `media-dir' subdirectories are
found.  Must end in a slash.")

(defvar media-dir "videos"
  "A directory or list of directories - relative to
`media-dir-prefix' - where media-files are found.")

;; what external command to execute to open media files
(defvar media-files-command-path '("vlc" "mplayer" "totem")
  "What program will `open-media-file' use to open a media file.
This can be either a string or a list of strings.  In the latter
case, it looks for each program and uses the first that is
found.")

(defvar media-file-regexp "\\(\\.avi$\\|\\.mp4$\\|\\.mpg$\\|\\.mpeg\\)")

(defvar media-users '(me))

(defstruct media-file path time users-watched episode-info)

(defvar *media-files* nil)

(defconst media-file-buffer " *media-files*")

(defvar media-files-filter-watched nil
  "If non-nil, then media files that have been watched by all
users in `media-users' will not be displayed in the media file
list.")

(defconst media-files-valid-sort-methods
  '(none filename timestamp series-and-episode)
  "Possible sorting methods.  Specifies which values of
`media-files-sort-by' and `media-files-sort-methods' are
allowed.")

(defvar media-files-sort-by 'series-and-episode
  "How to sort the list of media files.  See
`media-files-valid-sort-methods' for list of valid values.")

(defvar media-files-sort-methods media-files-valid-sort-methods
  "What sorting methods to use. `media-files-change-sort-method' uses this
list to cycle through the sort methods and set the value of
`media-files-sort-by'.  This should be some subset of
`media-files-valid-sort-methods', which contains the list of valid sort
methods.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions related to the media file display list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst media-files-assert-mode ()
  (assert (derived-mode-p 'media-files-mode)))

(defvar media-files-mode-map nil)
(unless media-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'media-file-next-item)
    (define-key map (kbd "<backtab>") 'media-file-previous-item) ;; on windows
    (define-key map (kbd "<S-tab>") 'media-file-previous-item)   ;; on os x
    (define-key map (kbd "f") 'media-file-next-item)
    (define-key map (kbd "b") 'media-file-previous-item)
    (define-key map (kbd "g") 'media-files-update)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "s") 'media-files-change-sort-method)
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
  (run-mode-hooks 'media-files-mode-hook)
  )

(defun display-media-files ()
  (interactive)
  (with-current-buffer (get-buffer-create media-file-buffer)
    (when (not (eq major-mode 'media-files-mode))
      (media-files-mode))
    (media-files-update))

  ;; TODO customize this behavior, support save and restore window config
  ;;(display-buffer media-file-buffer)
  (switch-to-buffer-other-window media-file-buffer)
  )

(defun media-files-update (&optional arg silent)
  "Revert the media files buffer to the contents of
`*media-files*' after applying any sorting or filters.  If
optional prefix ARG is non-nil, then first scan the filesystem
and update `*media-files*' using `update-media-files', which may
take a long time."
  (interactive "P")
  (media-files-assert-mode)
  (let ((line (line-number-at-pos (point)))
        media-files)
    (unless silent
      (message "Updating media list..."))
    (when arg (update-media-files))
    (setq media-files (mapcar 'copy-media-file *media-files*))
    (setq media-files (sort-media-files media-files))
    (toggle-read-only 0)
    (erase-buffer)
    (dolist (media-file media-files)
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
    (insert (format "%s" (media-file-episode media-file)))
    (add-text-properties beg-line (point)
            (list 'media-file media-file))
    (add-text-properties beg-file-name (point)
            (list 'mouse-face 'highlight
                  'keymap media-files-filename-map
                  'help-echo (media-file-base-name media-file))))
                  ;; 'help-echo "Middle click: open file"
  (insert " ") ;; keeps the mouse highlight from spilling over to next line
  (unless no-newline (newline)))

(defun media-files-change-sort-method (&optional sort-by)
  "Change the value of `media-files-sort-by' to SORT-BY.  If the current
buffer is a media file list buffer, then also update the list of media files
in the buffer with the new sort method.  This function can automatically
select the sort method from `media-files-sort-methods' or let the user
interactively choose it from the list `media-files-valid-sort-methods'.

SORT-BY must be one of the values in `media-files-valid-sort-methods', or
it can be `cycle' or nil.

If SORT-BY is `cycle' or nil, or if called interactively with a prefix arg,
then use the next sorting method in `media-files-sort-methods'.

When called interactively, user interactively selects the sort method from
`media-files-valid-sort-methods'.  The default choice is the next sort method
from `media-files-sort-methods', so calling this function interactively and
then hitting return is the same as passing `cycle' or a prefix arg."

  (interactive
   (list
    (if current-prefix-arg
        (media-files-next-sort-method)
        (intern
         (completing-read
          "Sort method: "
          (mapcar 'symbol-name media-files-valid-sort-methods)
          nil nil nil nil
          (symbol-name (media-files-next-sort-method)))))))

  ;; note: this won't work so well if media-files-sort-by isn't in
  ;; media-files-sort-methods
  (if (memq sort-by '(nil cycle))
      (setq sort-by (media-files-next-sort-method)))

  (when (null sort-by) (setq sort-by 'none))

  (if (not (memq sort-by media-files-valid-sort-methods))
      (message "Invalid sort method: %s" sort-by)

    (setq media-files-sort-by sort-by)
    (if (eq major-mode 'media-files-mode)
        (progn
          (message "Sorting by %s..." media-files-sort-by)
          (media-files-update nil t)
          (message "Sorting by %s...done" media-files-sort-by))
      (message "Media file sorting changed to %s" media-files-sort-by))))

(defun media-files-next-sort-method ()
  ;; if media-files-sort-by is not in media-files-sort-methods,
  ;; then the lookup will return nil, so we fall back to 'none.
  (or (cadr (memq media-files-sort-by media-files-sort-methods))
      'none))

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
    (when user
      (media-file-toggle-user-watched media-file user)

      ;; update the media file in the global list
      (mapc (lambda (x) (when (string= (media-file-path media-file)
                                       (media-file-path x))
                          (media-file-toggle-user-watched x user)))
            *media-files*)

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
    (media-files-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions related to the media-file data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun media-file-data (media-file)
  "Return the path and time of MEDIA-FILE as a pair."
  (cons (media-file-path media-file) (media-file-time media-file)))

(defun scan-media-dir (&optional dir)
  "Scan DIR recursively for all media files that match
`media-file-regexp'.  The result is a list of pairs of file names
and modification times. DIR can be a list, in which case each
directory in DIR is scanned and the results are accumulated into
a single list.  If DIR is nil, then use `media-dir'."
  (let ((dirs (cond ((null dir) media-dir)
                    ((listp dir) dir)
                    (t (list dir))))
        files)

    (setq files
          (apply 'append (mapcar (lambda (x) (directory-files-and-attributes-recursive (concat media-dir-prefix x) t media-file-regexp 'nosort)) dirs)))
    (mapcar (lambda (x) (cons (file-relative-name (car x) media-dir-prefix)
                              (nth 6 x)))
            files)))

(defun update-media-files (&optional dir)
  "Update `*media-files*'.  This adds new files, removes deleted
files, and updates the path and timestamp of any moved files
without changing the `media-file-users-watched' field.  See
`scan-media-dir' for description of DIR argument."
  (interactive)
  (let (prev-files files new-files dead-files moved-files)
    (setq prev-files (mapcar 'media-file-data *media-files*))

    ;; without this check, if media-dir-prefix isn't mounted,
    ;; then we'll end up *removing* all of the files in *media-files*
    ;; because we think they're dead files.
    (if (not (file-directory-p media-dir-prefix))
        (message "Directory does not exist: %s" media-dir-prefix)

      (setq files (scan-media-dir dir))

      (setq new-files (set-difference files prev-files :test 'equal))
      (setq dead-files (set-difference prev-files files :test 'equal))

      ;; detect moved files.
      ;; this might be very slow if a lot of files have been moved
      (setq moved-files nil)
      (dolist (x dead-files)
        (let ((y (find-if (lambda (y) (string= (file-name-nondirectory (car x)) (file-name-nondirectory (car y)))) new-files)))
          (when y
            (setq dead-files (delete x dead-files))
            (setq new-files (delete y new-files))
            (push (cons x y) moved-files))))

      (dolist (x new-files)
        (let ((media-file
               (make-media-file :path (car x) :time (cdr x))))
          (media-file-guess-info media-file)
          (push media-file *media-files*)))

      (dolist (x dead-files)
        (setq *media-files* (delete-if (lambda (y) (string= (media-file-path y) (car x))) *media-files*)))

      ;; update the path and time of each moved media file
      (dolist (x *media-files*)
        (let ((moved-file (assoc (media-file-data x) moved-files)))
          (when moved-file
            (setf (media-file-path x) (car (cdr moved-file)))
            (setf (media-file-time x) (cdr (cdr moved-file))))))

      (list (length new-files) (length dead-files) (length moved-files))
      )))

(defun sort-media-files (media-files &optional sort-by)
  "Sort MEDIA-FILES according to SORT-BY.  If nil, SORT-BY
defaults to `media-files-sort-by'."
  (when (null sort-by) (setq sort-by media-files-sort-by))
  (cond ((equal sort-by 'filename)
         (sort media-files 'media-file-name-lessp))
        ((equal sort-by 'timestamp)
         (sort media-files 'media-file-time-lessp))
        ((equal sort-by 'series-and-episode)
         (sort media-files 'media-file-episode-lessp))
        (t media-files)))

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
    (let ((command-paths (if (listp media-files-command-path)
                             media-files-command-path
                           (list media-files-command-path)))
          command-path)
      (setq command-path (find-if 'executable-find command-paths))
      (if command-path
          (call-process command-path nil 0 nil
                        (media-file-full-path media-file))
        (message "No media player found: %s" command-paths)))))

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

(defun media-file-episode-lessp (a b)
  (let ((x (media-file-episode-info a))
        (y (media-file-episode-info b)))
    (and (episode-p x) (episode-p y) (episode-lessp x y))))

(defun media-file-time-lessp (a b)
  (< 0 (float-time (time-subtract (media-file-time a) (media-file-time b)))))
  ;; (time-less-p (media-file-time b) (media-file-time a)))

(defun media-file-guess-info (media-file)
  (let ((result (guess-episode-info (media-file-path media-file))))
    (when result
      (setf (media-file-episode-info media-file) result))))

(defun media-file-series-name (media-file)
  (and (media-file-episode-info media-file)
       (episode-series-name (media-file-episode-info media-file))))

;; used for printing,
;; should be replaced with a function to pretty-print an episode.
(defun media-file-episode (media-file)
  (let ((episode (media-file-episode-info media-file)))
    (when episode
      (or (episode-date episode)
          (if (episode-number episode)
              (if (episode-season episode)
                  (cons (episode-season episode)
                        (episode-number episode))
                (episode-number episode)))
          (episode-title episode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun directory-files-and-attributes-recursive (directory &optional full match nosort id-format)
  (let (files)
    (dolist (x (directory-files-and-attributes directory full "^[^.]" nosort id-format))
      (if (eq t (cadr x)) ;; test if `x' is a directory
          (setq files (append (directory-files-and-attributes-recursive (car x) full match nosort id-format) files))
        (when (string-match match (file-name-nondirectory (car x)))
          (push x files))))
    files))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'media-files)
