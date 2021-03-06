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

;; information that is derived from a filename and series definition.
(defstruct episode series-name season number title date)

(defvar episode-number-regexp-alist
  '(("s\\(eason\\)?_?\\([0-9][0-9]?\\)_?e\\(p\\(isode\\)?\\)?_?\\([0-9][0-9]?\\)[^0-9]" 2 5)
    ("\\(season_?\\)?\\([0-9][0-9]?\\)/\\([0-9][0-9]?\\)[^0-9]" 2 3)
    ("\\([0-9][0-9]?\\)x\\([0-9][0-9]\\)[^0-9]" 1 2)
    ("\\([0-9][0-9]?\\)\\([0-9][0-9]\\)[^0-9]" 1 2))
  "The default alist to use for parsing filenames to determine
season and episode number.  Each entry contains three values: a
regular expression and two indices of the parenthesized
expressions of the regexp that correspond to the season and
episode number, respectively (see `string-match'). If only one
index is provided, then it is interpreted as the episode number,
and the season number is left as nil.")

(defvar episode-date-regexp-alist
  '(("\\([0-9][0-9][0-9][0-9]\\)\\.\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)" 1 2 3))
  "The default alist to use for parsing filenames to determine
the episode number of a show as a date.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; when episode-date-regexp is defined, use that (if t, use episode-date-regexp-alist),
;; otherwise, use episode-number-regexp or episode-number-regexp-alist.
(defun guess-episode-info (str)
  "Use `series-list' to guess the episode describes by
STR (usually a filename).  In the returned result, the car is the
name of the series and the cdr is the episode number, date, or
name.  The entire result can be nil, or it can be a series name
with a nil episode."
  (let ((series (match-series str series-list))
        episode date-result)
    (when series
      (setq episode (make-episode :series-name (series-name series)))
      (setq date-result (try-regexps str (get-series-episode-date-regexp series)))
      (if date-result
          (setf (episode-date episode) (mapcar 'string-to-number date-result))
        (let ((number (try-regexps str (get-series-episode-number-regexp series))))
          (cond ((= 1 (length number))
                 (setf (episode-number episode)
                       (string-to-number (car number))))
                ((= 2 (length number))
                 (setf (episode-season episode)
                       (string-to-number (car number)))
                 (setf (episode-number episode)
                       (string-to-number (cadr number))))))))
    episode))

(defun match-series (string &optional list)
  (when (not list) (setq list series-list))
  (find-if (lambda (x) (string-match (get-series-name-regexp x) string))
           series-list))

(defun get-series-name-regexp (series)
  (or (series-name-regexp series)
      (replace-regexp-in-string " " "." (series-name series))))

(defun get-series-episode-number-regexp (series)
  (or (series-episode-number-regexp series)
      episode-number-regexp-alist))

(defun get-series-episode-date-regexp (series)
  (if (eq t (series-episode-date-regexp series))
      episode-date-regexp-alist
  (series-episode-date-regexp series)))

(defun try-regexps (str regexp-alist)
  (unless (or (null regexp-alist) (listp (car regexp-alist)))
    (setq regexp-alist (list regexp-alist)))
  (let ((result (assoc-if (lambda (regexp) (string-match regexp str)) regexp-alist)))
    (if result
        (mapcar (lambda (x) (match-string x str)) (cdr result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compare episodes for sorting

(defun episode-lessp (x y)
  (let ((n (compare-episodes x y)))
    (and n (< n 0))))

;; returns nil if the episodes are not comparable, e.g. they don't have a series
;; name, or one uses an episode date while the other uses an episode title.

(defun episode-undef (x)
  (and (null (episode-date x))
       (null (episode-number x))
       (null (episode-title x))))

(defun compare-episodes (x y)
  (let ((name-x (episode-series-name x))
        (name-y (episode-series-name y))
        n)
    (when (and (stringp name-x) (stringp name-y))
      (setq n (compare-strings name-x 0 (length name-x)
                               name-y 0 (length name-y)
                               'case-insensitive))
      (if (eq n t)
          (let ((x-undef (episode-undef x))
                (y-undef (episode-undef y)))
            (cond ((and x-undef y-undef) 0)
                  (x-undef -1)
                  (y-undef 1)
                  (t (or (compare-episode-dates (episode-date x) (episode-date y))
                      (compare-episode-numbers (episode-season x) (episode-number x)
                                               (episode-season y) (episode-number y))
                      (compare-episode-titles (episode-title x) (episode-title y))))))
        n))))

(defun compare-episode-titles (x y)
  (and (stringp x)
       (stringp y)
       (compare-strings x 0 (length x) y 0 (length y))))

(defun compare-episode-numbers (season-x number-x season-y number-y)
  (and number-x number-y
       (let ((n (compare-numbers (or season-x 1) (or season-y 1))))
         (if (= n 0)
             (compare-numbers number-x number-y)
           n))))

(defun compare-numbers (x y)
  (cond ((< x y) -1)
        ((> x y) 1)
        (t 0)))

;; dates are (year month day)
(defun compare-episode-dates (x y)
  (when (and (= 3 (length x)) (= 3 (length y)))
    (compare-number-lists x y)))

(defun compare-number-lists (x y)
  (cond ((and (null x) (null y)) 0)
        ((or (null x) (and y (< (car x) (car y)))) -1)
        ((or (null y) (> (car x) (car y))) 1)
        (t (compare-number-lists (cdr x) (cdr y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'episode-names)
