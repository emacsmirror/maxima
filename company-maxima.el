;;; company-maxima.el --- Maxima company integration              -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Fermin Munoz

;; Author: Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Created: 5 October 2020
;; Version: 0.5.0
;; Keywords: languages,tools,convenience
;; URL: https://gitlab.com/sasanidas/maxima
;; Package-Requires: ((emacs "25.1") (seq "2.20")(company "0.9.13"))
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;; Commentary:
;; Maxima `company-mode' backend.
;; it uses the `maxima-mode' function `maxima-get-completions' with the
;; native apropos(); maxima function.

;; 

;;; Code:

;;;; The requires
(require 'company)
;; FIXME add maxima to MELPA dependencies
(require 'maxima)

(defun company-maxima-libraries (command &optional _arg &rest ignored)
  "Autocomplete maxima libraries inside load maxima function.
It requires COMMAND, optionally _ARG and IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-maxima-backend))
    (prefix (and (eq major-mode 'maxima-mode)
		 (company-in-string-or-comment)
		 ;; FIXME this could be improve, if the load string is in other line, it doesn't work
		 (string-match (rx (literal "load")(syntax open-parenthesis)(syntax string-quote)) (thing-at-point 'line t))
		 (company-grab-symbol)))
    (duplicates t)
    (candidates  (maxima-get-libraries (company-grab-symbol)))))

(defun company-maxima-symbols (command &optional _arg &rest ignored)
  "Company backend for `maxima-mode'.
It requires COMMAND, optionally _ARG and IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-maxima-backend))
    (prefix (and (eq major-mode 'maxima-mode)
		 (not (company-in-string-or-comment))
		 (company-grab-symbol)))
    (duplicates t)
    (candidates  (maxima-get-completions (company-grab-symbol)))))

(provide 'company-maxima)
;;; company-maxima.el ends here