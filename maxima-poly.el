;;; maxima-poly.el --- Polymode for Maxima                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Fermin Munoz

;; Author: Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Created: 8 Oct 2020
;; Version: 0.5.0
;; Keywords: languages, maxima,lisp
;; URL: https://gitlab.com/sasanidas/maxima
;; Package-Requires: ((emacs "25") (polymode "0.1.5") (maxima "0.5.0") )
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates Polymode and PHP.

;;; Code:
(require 'polymode)
(require 'maxima)

(defun maxima-poly-in-string-or-comment ()
  "Return non-nil if point is within a string or comment."
  (let ((ppss (syntax-ppss)))
    (or (car (setq ppss (nthcdr 3 ppss)))
        (car (setq ppss (cdr ppss)))
        (nth 3 ppss))))

(defconst maxima-poly--re-tag-tail-matcher
  (eval-when-compile
    (rx (syntax close-parenthesis)eol)))

(defun maxima-poly--tag-tail-matcher (ahead)
  "Matcher for tail of Lisp block, it requires AHEAD."
  (save-excursion
    (let ((re-search (if (< ahead 0) #'re-search-backward #'re-search-forward))
          found matched)
      (while (and (not found)
                  (setq matched (funcall re-search maxima-poly--re-tag-tail-matcher nil t)))
        (when (and matched (not (maxima-poly-in-string-or-comment)))
          (setq found (cons (match-beginning 0) (match-end 0)))))
      found)))


(define-hostmode maxima-mode-hostmode
  :mode 'maxima-mode)

(define-innermode lisp-innermode
  :mode 'common-lisp-mode
  :head-matcher (eval-when-compile
                  (rx (literal ":lisp") space (syntax open-parenthesis)))
  :tail-matcher #'maxima-poly--tag-tail-matcher
  :head-mode 'body
  :tail-mode 'body)

(define-polymode maxima-lisp-mode
  :hostmode 'maxima-mode-hostmode
  :innermodes '(lisp-innermode))

;;;###autoload
(autoload 'maxima-lisp-mode "maxima-lisp")

(provide 'maxima-poly)
;;; maxima-poly.el ends here
