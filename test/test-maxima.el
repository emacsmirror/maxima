;;; test-maxima.el --- Maxima test file.               -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Fermin Munoz

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
;; You will need both maxima.el and maxima-font-lock.el

;;; Commentary:
;; Maxima test file.
;; Eval this command to run the test or eval this buffer
;; (test-simple-run "emacs -Q --batch -L %s  -L %s -l %s -l %s"
;; 		 (file-name-directory (locate-library "maxima.el"))
;; 		 (file-name-directory (locate-library "test-simple.el"))
;; 		 (locate-library "s.el")
;; 		 buffer-file-name)

;;; Code:

;;;; The requires
(require 'test-simple)
(require 'maxima)
(require 's)

(test-simple-start)

(note "Maxima command")

(assert-t (executable-find maxima-command)
	  "Can't find maxima executable.")

(note "Maxima version")

(assert-t (stringp (maxima-get-version 'maxima-command))
	  "The command `maxima-get-version' doesn't return a string.")

(note "Maxima library")

(assert-t (file-directory-p maxima-libraries-directory)
	  "The library directory doesn't exist.")

(note "Maxima string functions")

(setq-local maxima-string-example
	    "/* comment */ sqrt(4) /* comment */")

(assert-equal "sqrt(4) /* comment */" (maxima-strip-string-beginning maxima-string-example)
	      "Error in `maxima-strip-string-beginning'.")

(assert-equal "/* comment */ sqrt(4)" (maxima-strip-string-end maxima-string-example)
	      "Error in `maxima-strip-string-end'.")

(assert-equal "sqrt(4)" (maxima-strip-string maxima-string-example)
	      "Error in `maxima-strip-string'.")

(assert-equal "sqrt(4);" (maxima-strip-string-add-semicolon maxima-string-example)
	      "Error in `maxima-strip-string-add-semicolon'.")

(note "Maxima query position functions")

(assert-t (with-temp-buffer
	    (maxima-mode)
	    (insert "/* this is a comment */")
	    (re-search-backward "this")
	    (maxima-in-comment-p))
	  "Error in `maxima-in-comment-p'.")

(note "Maxima query position functions")

(assert-equal ";" (with-temp-buffer
		    (maxima-mode)
		    (insert "sqrt(4);")
		    (re-search-backward "sq")
		    (maxima-re-search-forward ";" nil))
	      "Error in `maxima-re-search-forward'.")

(assert-equal ";"
	      (with-temp-buffer
		(maxima-mode)
		(insert "sqrt(4)/* comment ; */ ;")
		(re-search-backward "sq")
		(maxima-re-search-forward-skip-blocks ";" nil))
	      "Error founding ; with `maxima-re-search-forward-skip-blocks'.")

(assert-nil (with-temp-buffer
	      (maxima-mode)
	      (insert "sqrt(4)/* comment ; */")
	      (re-search-backward "sq")
	      (maxima-re-search-forward-skip-blocks ";" nil))
	    "Error, a ; was found when it wasn't suppose to with
`maxima-re-search-forward-skip-blocks'.")


(assert-equal ";" (with-temp-buffer
		    (maxima-mode)
		    (insert "sqrt(4);")
		    (maxima-re-search-backward ";" nil))
	      "Error in `maxima-re-search-backwards'.")

(assert-equal ";"
	      (with-temp-buffer
		(maxima-mode)
		(insert "sqrt(4)/* comment ; */ ;")
		(maxima-re-search-backward-skip-blocks ";" nil))
	      "Error founding ; with `maxima-re-search-backward-skip-blocks'.")

(assert-nil (with-temp-buffer
	      (maxima-mode)
	      (insert "sqrt(4)/* comment ; */")
	      (maxima-re-search-backward-skip-blocks ";" nil))
	    "Error, a ; was found when it wasn't suppose to with
`maxima-re-search-backward-skip-blocks'.")


(note "Maxima move position functions")


(assert-equal 27
	      (with-temp-buffer
		(maxima-mode)
		(insert " /* this is a comment */  sqrt(4);")
		(move-beginning-of-line 0)
		(maxima-forward-over-comment-whitespace)
		(point))
	      "Error with `maxima-forward-over-comment-whitespace'.")

(assert-equal 10 (with-temp-buffer
		   (maxima-mode)
		   (insert "sqrt(4);a  /* this is a comment */")
		   (maxima-back-over-comment-whitespace)
		   (point))
	      "Error with `maxima-back-over-comment-whitespace'.")

(assert-equal 9 (with-temp-buffer
		  (maxima-mode)
		  (insert "sqrt(4);a  /* this is a comment */")
		  (maxima-goto-beginning-of-form)
		  (point))
	      "Error with `maxima-goto-beginning-of-form'.")

(assert-equal 9 (with-temp-buffer
		  (maxima-mode)
		  (insert "sqrt(4);a  /* this is a comment */")
		  (re-search-backward "qr")
		  (maxima-goto-end-of-form)
		  (point))
	      "Error with `maxima-goto-end-of-form'.")

(assert-equal 9 (with-temp-buffer
		  (maxima-mode)
		  (insert "sqrt(4);a  /* this is a comment */")
		  (re-search-backward "qr")
		  (maxima-goto-end-of-expression)
		  (point))
	      "Error with `maxima-goto-end-of-expression'.")

(assert-equal 'if (with-temp-buffer
		    (maxima-mode)
		    (insert
		     "if sqrt(4) = 2 then
		   print(\"Correct\")
		   else
		   print(\"No\");")
		    (re-search-backward "2")
		    (maxima-goto-beginning-of-construct (point-min))
		    (symbol-at-point))
	      "Error with `maxima-goto-beginning-of-construct'.")


(note "Inferior process functions")

(assert-t (progn
	    (maxima-init-inferiors)
	    (and maxima-inferior-process maxima-auxiliary-inferior-process))
	  "`maxima-inferior-process' doesn't start correctly.")

(assert-nil (progn
	      (maxima-stop t)
	      (and (not (get-buffer "*maxima*" )) (not (get-buffer "*aux-maxima*"))
		   (and (processp maxima-inferior-process)
			(processp maxima-auxiliary-inferior-process))))
	    "`maxima-stop' doesn't stop the processes correctly.")

(assert-equal (list "test" t)
	      (let* ((inferior (maxima-make-inferior "test" t))
		     (inferior-name (process-name inferior)))
		(list inferior-name (processp inferior)))
	      "`maxima-make-inferior' doesn't returns a process
	      with the correct name.")

(assert-t (let* ((inferior-process (maxima-make-inferior "test" t))
		 (inferior-buffer (process-buffer inferior-process)))
	    (maxima-remove-inferior inferior-process)
	    (and (not (buffer-name inferior-buffer))
		 (equal (process-status inferior-process) 'signal)))
	  "`maxima-remove-inferior' doesn't delete the inferior
	  buffer and process correctly.")

(note "Help functions")

(assert-equal '("sqrt" "sqrtdispflag")
	      (maxima-get-completions "sqrt"))
(assert-t
 (progn
   (maxima-get-info-on-subject "sqrt" t)
   (bufferp (get-buffer "*maxima-help*")))
 "`maxima-get-info-on-subject' doesn't create a help buffer.")

(assert-equal "sqrt (<x>)"
	      (with-temp-buffer
		(maxima-mode)
		(insert "sqrt")
		(goto-char (point-min))
		(maxima-symbol-doc))
	      "`maxima-symbol-doc' doesn't return the correct symbol signature.")

(assert-equal '("sqrt (<x>)")
	      (progn
		(maxima-init-inferiors)
		(sleep-for 0.4)
		(maxima-document-get "sqrt"))
	      "`maxima-document-get' doesn't return the correct symbol list.")



(end-tests)

(provide 'test-maxima)
;;; test-maxima.el ends here
