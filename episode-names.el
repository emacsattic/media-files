;; episode-names.el
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
;; Support for guessing the name of a TV show and episode number/title from the
;; filename.
;;
;; An episode number/title can be any of the following
;;   - a single episode title or number
;;   - a season number paired with an episode number or title
;;   - a date (year month day)
;;
;; - if name-regexp is nil, regexp is derived from name.
;; - if episode-number-regexp is nil, default to the global
;;   episode-number-regexp-alist.
;; - if date-regexp is non-nil, use it instead of episode-regexp
;;
;; TODO
;; - support for episode titles
;; - some shows have DVD extras or opening credits
;; - some shows have special episodes/movies (Battlestar Galactica, Futurama,
;;   Extras, South Park) that have a name instead of a number

(defstruct series name name-regexp episode-number-regexp episode-date-regexp)

(defvar series-list nil)

(defvar episode-number-regexp-alist
  '(("s\\(eason\\)?_?\\([0-9][0-9]?\\)_?e\\(p\\(isode\\)?\\)?_?\\([0-9][0-9]?\\)" 2 5)
    ("\\(season_?\\)?\\([0-9][0-9]?\\)/\\([0-9][0-9]?\\)" 2 3)
    ("\\([0-9][0-9]?\\)x?\\([0-9][0-9]\\)" 1 2))
  "An alist for parsing filenames to determine season and episode
number.  Each entry contains three values: a regular expression
and two indices of the parenthesized expressions of the regexp
that correspond to the season and episode number,
respectively (see `string-match').")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun guess-episode-info (str)
  "Use `series-list' to guess the episode describes by
STR (usually a filename).  In the returned result, the car is the
name of the series and the cdr is the episode number, date, or
name.  The entire result can be nil, or it can be a series name
with a nil episode."
  (let (name-result episode-result)
    (loop for x in series-list until name-result do
          (setq name-result
                (and (string-match (or (series-name-regexp x)
                                       (make-series-regexp (series-name x)))
                                   str)
                     (series-name x)))

          (when name-result
            (let ((regexps (or (series-episode-date-regexp x) (series-episode-number-regexp x) episode-number-regexp-alist)))
              (setq episode-result
                    (try-regexps str regexps)))))

    (when name-result
      (cons name-result episode-result))))

(defun make-series-regexp (name)
  (replace-regexp-in-string " " ".*" name))

(defun try-regexps (str regexp-alist)
  (unless (or (null regexp-alist) (listp (car regexp-alist)))
    (setq regexp-alist (list regexp-alist)))
  (let ((result (assoc-if (lambda (regexp) (string-match regexp str)) regexp-alist)))
    (if result
        (mapcar (lambda (x) (match-string x str)) (cdr result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'episode-names)
