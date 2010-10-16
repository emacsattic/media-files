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

(require 'cl)

;; TODO support for episode title regexp
;; user-defined data for a series
(defstruct series name name-regexp episode-number-regexp episode-date-regexp)

(defvar series-list nil)

;; information that is derived from a filename and series definition
(defstruct episode series-name season number title date)

(defvar episode-number-regexp-alist
  '(("s\\(eason\\)?_?\\([0-9][0-9]?\\)_?e\\(p\\(isode\\)?\\)?_?\\([0-9][0-9]?\\)[^0-9]" 2 5)
    ("\\(season_?\\)?\\([0-9][0-9]?\\)/\\([0-9][0-9]?\\)[^0-9]" 2 3)
    ("\\([0-9][0-9]?\\)x\\([0-9][0-9]\\)[^0-9]" 1 2)
    ("\\([0-9][0-9]?\\)\\([0-9][0-9]\\)[^0-9]" 1 2))
  "An alist for parsing filenames to determine season and episode
number.  Each entry contains three values: a regular expression
and two indices of the parenthesized expressions of the regexp
that correspond to the season and episode number,
respectively (see `string-match'). If only one index is provided,
then it is interpreted as the episode number, and the season
number is left as nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun guess-episode-info (str)
  "Use `series-list' to guess the episode describes by
STR (usually a filename).  In the returned result, the car is the
name of the series and the cdr is the episode number, date, or
name.  The entire result can be nil, or it can be a series name
with a nil episode."
  (let (episode series-name)
    (loop for x in series-list until episode do
          (setq series-name
                (and (string-match (or (series-name-regexp x)
                                       (make-series-regexp (series-name x)))
                                   str)
                     (series-name x)))

          (when series-name
            (setq episode (make-episode :series-name series-name))
            (setf (episode-date episode)
                  (mapcar 'string-to-number
                          (try-regexps str (series-episode-date-regexp x))))
            (unless (episode-date episode)
              (let ((number (try-regexps str (or (series-episode-number-regexp x)
                                                 episode-number-regexp-alist))))
                (cond ((= 1 (length number))
                       (setf (episode-number episode)
                             (string-to-number (car number))))
                      ((= 2 (length number))
                       (setf (episode-season episode)
                             (string-to-number (car number)))
                       (setf (episode-number episode)
                             (string-to-number (cadr number)))))))))
    episode))

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
