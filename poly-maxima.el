;;; poly-maxima.el --- Polymode for Maxima                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Fermin Munoz

;; Author: Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Created: 8 Oct 2020
;; Version: 0.7.6
;; Keywords: languages, maxima,lisp
;; URL: https://gitlab.com/sasanidas/maxima
;; Package-Requires: ((emacs "25") (polymode "0.1.5") (maxima "0.6.0") )
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

;; This package integrates Polymode with maxima major mode.
;; It uses the "special" symbol /*el*/ for delimiter.
;; You can test this feature using `poly-maxima-insert-block', will create a correct syntax block
;;
;; There is also important to remember that the Lisp code must be contracted at the end
;; because maxima only accept one-line Lisp code.

;;; Code:
(require 'polymode)
(require 'maxima)

(defun poly-maxima-insert-block ()
  "Insert a :lisp code with the correct `poly-maxima' syntax."
  (interactive)
  (let* ((current-point (point)))
    (insert ":lisp \n/*el*/")
    (goto-char (+ current-point 6))))

(defun poly-maxima-contract-lisp ()
  "Handy function to contract into a single line the Lisp code."
  (interactive)
  (when (or (eq major-mode 'common-lisp-mode)
   	    (eq major-mode 'lisp-mode))
    (let* ((init-point (re-search-backward (rx bol (literal ":lisp")) nil t))
	   (end-point (- (re-search-forward "/\\*el\\*/" nil t) 7) ))
      (narrow-to-region init-point end-point)
      (goto-char (point-min))
      (while (re-search-forward (rx (or (regexp "\t") (regexp "\n"))) nil t)
	(replace-match ""))
      (widen))))

(defun poly-maxima-in-string-or-comment ()
  "Return non-nil if point is within a string or comment."
  (let ((ppss (syntax-ppss)))
    (or (car (setq ppss (nthcdr 3 ppss)))
        (car (setq ppss (cdr ppss)))
        (nth 3 ppss))))

(defconst poly-maxima--re-tag-tail-matcher
  (eval-when-compile
    (rx (literal "/*el*/"))))

(defun poly-maxima--tag-tail-matcher (ahead)
  "Matcher for tail of Lisp block, it requires AHEAD."
  (save-excursion
    (let ((re-search (if (< ahead 0) #'re-search-backward #'re-search-forward))
          found matched)
      (while (and (not found)
                  (setq matched (funcall re-search poly-maxima--re-tag-tail-matcher nil t)))
        (when (and matched (not (poly-maxima-in-string-or-comment)))
          (setq found (cons (match-beginning 0) (match-end 0)))))
      found)))


(define-hostmode maxima-mode-hostmode
  :mode 'maxima-mode)

(define-innermode lisp-innermode
  :mode 'common-lisp-mode
  :head-matcher (eval-when-compile
                  (rx (literal ":lisp")))
  :tail-matcher #'poly-maxima--tag-tail-matcher
  :head-mode 'body
  :tail-mode 'body)

(define-polymode poly-maxima
  :hostmode 'maxima-mode-hostmode
  :innermodes '(lisp-innermode))

;;;###autoload
(autoload 'poly-maxima "poly-maxima")

(provide 'poly-maxima)
;;; poly-maxima.el ends here
