;;; MHS-COMMENT.EL --- allows user entering of useful parts of c subroutine comments.

;; Copyright (C) 1997 Matthew H. Savoie

;; Author: Matthew H. Savoie <savoie@fsl.noaa.gov>
;; Maintainer: Matthew H. Savoie <savoie@fsl.noaa.gov>
;; Created: 11 Jun 1997
;; Version: 1.0
;; Keywords:

;;; Commentary:
;;;  to use, place this file in your load-path, byte compile it and
;;;  put (require 'mhs-comment) in your .emacs file

;;; You can call the program interactively with `M-x
;;; mhs-c-sub-comment' or bind this to a keystroke.
;;; (global-set-key [(control c)(alphakey)] 'mhs-c-sub-comment)

;;; Limitations:
;;;   to use this routine, the point must be located in the top line
;;;   of the function declaration.
;;;
;;;   Function return type declaration must be on one line (but the
;;;   arguments need not be) : (e.g.
;;;   returntype[ ][*] [class::]FunctionName[ ]+( [arguments
;;;   separated by commas])  )

;;; int* qcLine::WhichOne(int I, int J,
;;;                       othertype moreargs)


;;; change log
;;; $Log: not supported by cvs2svn $
;;; Revision 1.13  1997/09/18 14:53:48  savoie
;;; last before emacs 20.1
;;;
;;; Revision 1.12  1997/07/08 15:15:09  savoie
;;; fixed some references that were missing
;;;
;;; Revision 1.11  1997/06/27 20:58:56  savoie
;;; takes care of voids for real.
;;;
;;; Revision 1.10  1997/06/27 19:26:59  savoie
;;; fixed problems with void arguments
;;;
;;; Revision 1.9  1997/06/26 21:05:33  savoie
;;; working version
;;;


;;; Code:

(defconst mhs-comment-version (substring "$Revision: 19387 $" 11 -2)
  "$Id: mhs-comment.el 19387 2011-09-18 16:23:58Z savoie $
 
Report bugs to: Matthew H. Savoie <savoie@fsl.noaa.gov>")

(defun mhs-fetch-last-word ()
  (let (begin end word)
  (save-excursion
    ;; check for white-space
    (setq end (point))
    (forward-char -1)
    (while (looking-at " ") 
      (forward-char -1))
    (setq end (1+ (point)))
    (while (or (not (looking-at "[ :]" )) (bobp)) 
      (forward-char -1))
    (setq begin (1+ (point))))
  (setq word (buffer-substring begin end))
  (set-text-properties 0 (length word) nil word)
  word ))


(defun mhs-insert-the-comment (var-name parm-list)
  (let ((top-line    (concat "/*" (make-string 70  ?-) "\n"))
	(bottom-line (concat " *" (make-string 70  ?-) "*/"))
	(pref " * ")
	(empty-line  " * \n")
	(types '("output" "result" "effect" "note" "refer"))
	(type-notes '("description of output parameter"
		      "description of return value"
		      "global data modified, etc."
		      "special considerations [especially side-effects]"
		      "any references [books, articles, papers]"))
	;; local vars.
	type l-ptr d-ptr prom here goal fix-last begin)

    (insert  top-line)
    (insert bottom-line)
    (beginning-of-line)
    (insert pref " " var-name " - ")
    (setq begin (point))
    (setq fill-prefix
	  (concat " *"
		  (make-string (- (current-column) 2) (string-to-char " "))))
    (insert (read-string "short desc of subroutine: "))
    (fill-region begin (point) 'nil 't)

    ;;insert the Variable input paramater lines here.

    (setq l-ptr parm-list)
    (setq here (concat pref "    input : "))
    (cond ((not (equal l-ptr 'nil))
	   (insert empty-line)
	   (insert  here)))

    ;; while there are variables to document
    (while (not (equal l-ptr 'nil))
      (setq type (car l-ptr))
      (setq prom (concat type ": "))
      (insert type " - ")
      (setq fill-prefix
	    (concat " *"
		    (make-string
		     (- (current-column) 2) (string-to-char " "))))
      (setq begin (point))
      (insert (read-string prom ""))
      (fill-region begin (point) 'nil 't)
      (if (not (equal (cdr l-ptr) 'nil))
	  (insert " *"
		  (make-string (- (length here) 2)(string-to-char " "))))
      (setq l-ptr (cdr l-ptr)))

    ;; insert the other types for the comments.
    (setq l-ptr types)
    (setq d-ptr type-notes)
    (insert empty-line)
    (while (not (equal l-ptr 'nil))
      (setq type (car l-ptr))
      (setq type-notes (car d-ptr))
      (setq prom (concat  type-notes " (" type "): "))
      (setq here (read-string prom))
      (cond ((not (equal here ""))
	     (insert (concat pref "   " type ": "))
	     (setq goal (current-column))
	     (setq begin (point))
	     (setq fill-prefix
		  (concat " *" (make-string (- goal 2) (string-to-char " "))))
	     (insert here)
	     (fill-region begin (point) 'nil 't)))
      (setq l-ptr (cdr l-ptr))
      (setq d-ptr (cdr d-ptr)))))



(defun get-var-param ()
  (save-restriction
    (let (var-list thisvar begin end 
		    (vars-p 't)
		    (begin-par "(")
		    (end-par ")"))
      (beginning-of-buffer)
      (while (not (looking-at begin-par)) (forward-char))
      (setq begin (point))
      (while (not (looking-at end-par)) (forward-char))
      (if (not (eobp)) (forward-char))
      (narrow-to-region begin (point))
      (end-of-buffer)
      
      (if (not (search-backward-regexp "\\w[ ]+\\w" 'nil 't 'nil))
	  (setq vars-p 'nil))
      
      (cond ( vars-p
	     (end-of-buffer)
	     (while (not (looking-at end-par)) (backward-char))
	     (setq var-list (cons (mhs-fetch-last-word) 'nil))
	     (while  (search-backward "," (point-min) 't 'nil)
	       (setq thisvar (mhs-fetch-last-word))
	       (setq var-list (cons thisvar var-list))))
	    (setq var-list 'nil))
      var-list)))
      

(defun mhs-c-sub-comment()
  "*Interactively adds a C comment in nsidc style.  To run the program
  make sure the cursor is within the function header (before the
  left parenthesis) before calling this routine."
  (interactive)
  (let ( begin end  var-list  fun-name
	       (end-par ")")
	       (begin-par "("))

    (save-excursion
      (if (not (search-forward 
		begin-par
		(save-excursion (end-of-line)(point)) 't 'nil))
	  (while (not (looking-at begin-par))(backward-char)))
      (beginning-of-line)
      (setq begin (point))
      (search-forward end-par)
      (save-restriction
	(narrow-to-region begin (point))
	(setq end (point))
	(goto-char begin)
	(search-forward begin-par)
	(backward-char)
	(setq begin (point))
	(setq fun-name (mhs-fetch-last-word))
	(setq var-list (get-var-param))
	(save-restriction
	  (narrow-to-region begin (point-max))))
      (beginning-of-line)
      (insert "\n")
      (forward-line -1)      
      (mhs-insert-the-comment fun-name var-list))))

(provide 'mhs-comment)
;;; mhs-comment ends here
