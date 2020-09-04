;;; maxima-noweb.el --- Maxima minor mode for noweb-mode interaction              -*- lexical-binding: t; -*-

;; Copyright (C) 1998,1999 William F. Schelter
;; Copyright (C) 2001-2007 Jay Belanger
;; Copyright (C) 2020 Fermin Munoz

;; Author: William F. Schelter
;;         Jay Belanger
;;         Fermin Munoz<fmfs@posteo.net>
;; Created: 30 April 2020
;; Version: 0.5.0
;; Keywords: maxima,tools,math
;; URL: https://gitlab.com/sasanidas/maxima
;; Package-Requires: ((emacs "25.1") (seq "2.20"))
;; License: GPL-3.0-or-later

;;; Commentary:

;; Maxima minor mode for noweb-mode interaction.

;;; Code:

;;;; The requires

(require 'maxima)


(defvar maxima-noweb-ignore-bounds '("<<" ">>"))

(defun maxima-noweb-in-ignore-bounds-p ()
  "Check if the maxima code is inside ignore bound."
  (if (not maxima-noweb-ignore-bounds)
      nil
    (let ((pt (point)))
      (save-excursion
        (if (not (re-search-backward (car maxima-noweb-ignore-bounds) nil t))
            nil
          (not (re-search-forward (cadr maxima-noweb-ignore-bounds) pt t)))))))

(defun maxima-noweb-forward-out-of-ignore-bounds (&optional pmax)
  "Internal noweb funtion, it has an optional argument PMAX."
  (re-search-forward (cadr maxima-noweb-ignore-bounds) pmax 1))

(defun maxima-noweb-backward-out-of-ignore-bounds (&optional pmin)
  "Internal noweb fuction, it has an optional argument PMIN."
  (re-search-backward (car maxima-noweb-ignore-bounds) pmin 1))


(defun maxima-noweb-re-search-forward (regexp &optional pmax)
  "Search forward for REGEXP, bounded by PMAX.
Ignore matches found in comments and strings, and skip over
parenthesized or bracketed blocks."
  (let ((match
         (maxima-standard-re-search-forward regexp pmax)))
    (while (maxima-noweb-in-ignore-bounds-p)
      (maxima-noweb-forward-out-of-ignore-bounds pmax)
      (setq match
            (maxima-standard-re-search-forward regexp pmax)))
    match))

(defun maxima-noweb-re-search-backward (regexp &optional pmin)
  "Search backward for REGEXP, bounded by PMIN.
Ignore matches found in comments and strings."
  (let ((match
         (maxima-standard-re-search-backward regexp pmin)))
    (while (maxima-noweb-in-ignore-bounds-p)
      (maxima-noweb-backward-out-of-ignore-bounds pmin)
      (setq match
            (maxima-standard-re-search-backward regexp pmin)))
    match))

(defun maxima-noweb-next-char-word-part-p ()
  "Non-nil if next char is a a word part."
  (or
   (looking-at "\\w")
   (looking-at "\\\\")
   (and
    (looking-at ">")
    (save-excursion
      (forward-char -1)
      (looking-at ">")))
   (save-excursion
     (forward-char -1)
     (looking-at "\\\\"))))

(defun maxima-noweb-forward-word ()
  "Go to the end of the current word."
  (if (maxima-noweb-in-ignore-bounds-p)
      (maxima-noweb-forward-out-of-ignore-bounds))
  (let ((keep-going t))
    (while keep-going
      (cond
       ((looking-at "\\w")
        (forward-word 1))
       ((looking-at "\\\\")
        (forward-char 2))
       ((looking-at "<<")
        (forward-char 2)
        (maxima-noweb-forward-out-of-ignore-bounds))
       (t
        (setq keep-going nil))))))

(defun maxima-noweb-backward-word ()
  "Go to the beginning of the current word."
  (if (maxima-noweb-in-ignore-bounds-p)
      (maxima-noweb-backward-out-of-ignore-bounds))
  (let ((keep-going t))
    (while keep-going
      (cond
       ((and
         (> (point) (point-min))
         (save-excursion
           (forward-char -1)
           (looking-at "\\w")))
        (backward-word 1))
       ((and
         (> (point) (1+ (point-min)))
         (save-excursion
           (forward-char -2)
           (looking-at "\\\\")))
        (forward-char -2))
       ((and
         (> (point) (1+ (point-min)))
         (save-excursion
           (forward-char -2)
           (looking-at ">>")))
        (forward-char -2)
        (maxima-noweb-backward-out-of-ignore-bounds))
       (t
        (setq keep-going nil))))))

(defun maxima-noweb-goto-beginning-of-form ()
  "Move to the beginning of the form."
  (if (re-search-backward "^<<.*?>>= *$" (point-min) 1)
      (forward-line 1))
  (maxima-forward-over-comment-whitespace))


(defun maxima-noweb-goto-end-of-form ()
  "Move to the end of the form."
  (when (re-search-forward "\\(^@\\( \\|$\\)\\|^<<.*>>= *$\\)" nil 1)
    (forward-line -1)
    (end-of-line)
    (maxima-back-over-comment-whitespace)))

(defun maxima-noweb-perhaps-smart-calculate-indent ()
  "Try to calculate the indentation."
  (let ((indent nil)
        (pt))
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at "^<<.*?>>=[ \t]*$")
        (setq indent -1))
       ((looking-at "^@[ \n]")
        (setq indent -1))
       (t
        (forward-line -1)
        (if (looking-at "^<<.*?>>=[ \t]*$")
            (setq indent -1)))))
    (if indent
        indent
      (maxima-standard-perhaps-smart-calculate-indent))))

(define-derived-mode maxima-noweb-mode maxima-mode
  "Maxima Noweb Mode"
  (setq maxima-mode-type 'maxima-noweb-mode))

(provide 'maxima-noweb)
;;; maxima-noweb.el ends here
