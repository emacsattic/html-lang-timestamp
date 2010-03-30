; -*- Mode: Emacs-Lisp -*- 
;;; html-lang-timestamp.el --- Set language specific timestamps
;;;                            in HTML buffers.

;; Revision 1.0
;; last edited on 4.6.1998

;; Copyright (C) 1998 Holger Schauer

;; Author: Holger Schauer <Holger.Schauer@gmx.de>
;; Keywords: utils html time

;; This file is not part of Emacs.

;; Developed under XEmacs 20.4. Should work with most other Emacs-sen.
;; Intended to be used in conjunction with psgml.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; If you setq this into `html-helper-timestamp-hook' you will get
;; language dependent timestamps. "german" and "english" are predefined,
;; using the "normal" current-time-string function.

;; Of course, you can specify whatever style you want.
;; Say you like to have a style "holgers-horror-timestamp", you could do:
;; (pushnew '("holgers-horror" 
;;            "Raised from the grave at %s minutes past midnight."
;;            holgers-minutes-after-midnight-calculator)
;;          html-lang-timestamp-default-language)
;; and voila. "german" and "english" are predefined, using the
;; "normal" current-time-string function.

;; If you like a specific style to be used by default, you can do
;; (setq html-lang-timestamp-default-language "english")
;; If you want to change the style of a current document, use
;; M-x html-lang-timestamp-change-lang.

(require 'cl)
;;; Some compatibilty stuff

;; stolen from replace.el from XEmacs 20.4
(if (not (fboundp 'match-string))
    (defun match-string (num &optional string)
      "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
      (if (match-beginning num)
	  (if string
	      (substring string (match-beginning num) (match-end num))
	    (buffer-substring (match-beginning num) (match-end num))))))

 
(defvar html-lang-timestamp-default-language nil
  "Sets the default language in which the date should be inserted.

 NIL means don't insert a date.")

(defvar html-lang-timestamp-string-list
  '(("german" "Zuletzt verändert am: %s")
    ("english" "Last modified on %s"))
  "Specifies the texts used for creating the timestamp.

 It is a list of cons cells of the form '(LANG TEXT FUNCTION), where
 LANG and TEXT are both strings. If TEXT contains a `%s', the
 current date and time will be inserted there. If FUNCTION is unspecified,
 `current-time-string' will be used to compute the curent date and time, 
 otherwse FUNCTION will be called.")



(defconst html-lang-timestamp-html-comment-start "<!--")
(defconst html-lang-timestamp-html-comment-end "-->")

(defun html-lang-timestamp-create-datestring (lang)
 "Creates the date-string."
 (do ((ltsl html-lang-timestamp-string-list (setq ltsl (cdr ltsl))))
     ((or (null ltsl)
	  (string-match (car (car ltsl)) lang))
      (setq date-string 
	    (unless (null ltsl)
	      (format (nth 1 (car ltsl)) ;; the modified string
		      (if (null (nth 2 (car ltsl))) ;; if unspecified
			  (current-time-string)     ;; use default
			(funcall (nth 2 (car ltsl)))))))))) ;; else use spec.
 

(defun html-lang-timestamp ()
  "Check the language and insert date.

  See also the documentation of the variable 
 `html-lang-timestamp-string-list'."  
  (interactive)
  (let (lang date-string)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp
	   (concat 
	    "^"
	    html-lang-timestamp-html-comment-start
	    " doc-lang: "
	    "\\(\\w+\\).*" ; a word and then whatever
	    html-lang-timestamp-html-comment-end)
	   (point-max)
	   t)
	  (setq lang (match-string 1))))

    (if (or lang
	    (setq lang html-lang-timestamp-default-language))
	(setq date-string (html-lang-timestamp-create-datestring lang)))
    
    (unless date-string
      (message "Document language unknown.")
      (setq lang (read-from-minibuffer "Document language: ")))

    (when lang
	(setq date-string (html-lang-timestamp-create-datestring lang)))

    (when date-string
      (insert date-string "\n")
      ;; and now insert the language - outside of hhmts comment !
      (cond ((and (goto-char (point-min))
		  (search-forward-regexp
		   (concat 
		    "^"
		    html-lang-timestamp-html-comment-start
		    " doc-lang: "
		    "\\(\\w+\\).*" ; a word and then whatever
		    html-lang-timestamp-html-comment-end)
		   (point-max)
		   t))
	     (goto-char (match-beginning 0))
	     (beginning-of-line)
	     (kill-line))
	    ((and (goto-char (point-min))
		  (search-forward "</body>" (point-max) t))
	     (goto-char (match-beginning 0))
	     (beginning-of-line)
	     (newline)
	     (previous-line 1)))
      
      (insert (concat 
	       html-lang-timestamp-html-comment-start
	       " doc-lang: "
	       lang " "
	       html-lang-timestamp-html-comment-end)))))

(defun html-lang-timestamp-change-lang ()
  "Change language of current buffe, also inserting new modified date."
  (interactive)
  (let ((lang (read-from-minibuffer "Document language: ")))
    (make-variable-buffer-local 'html-lang-timestamp-default-language)
    (setq html-lang-timestamp-default-language lang)
    (message "Set document language to: %s" 
	     html-lang-timestamp-default-language)))

(provide 'html-lang-timestamp)



