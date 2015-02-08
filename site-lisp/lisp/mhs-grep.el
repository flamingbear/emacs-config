;;
;; These functions alow me the quick access to my own grep commands.  They
;; will grep for the highlighted words and search all files with the extensions
;;
;; $Log: not supported by cvs2svn $
;; Revision 0.2  1997/09/18 14:54:01  savoie
;; last before emacs 20.1
;;
;; Revision 0.1  1996/12/03 18:35:52  savoie
;; *** empty log message ***
;;

(defun mhs-grep (all &optional o istr) "\
  *Will grep for the region selected in all files with a similiar 
    root. i.e. `.pro', `*.c'"
  (interactive)
  (let (here there sstring filestr grepstring opts )

    ;;(here there sstring grepstring tbuf dotloc)
    (save-excursion)
    (if  (not istr) 
	(setq sstring (buffer-substring (point) (mark))) 
      (setq sstring istr)) 

    ;; use opts of they are available
    (if (not o)
	(setq opts "-n") (setq opts (concat "-n" o)))

    ;; find buffer extension
    (if (not all)
	;; if only one file, search buffer name
	(setq filestr (buffer-name))
      ;; otherwise search all files with same extension
      (setq filestr 
	    (concat "*"
		    (substring (buffer-name) 
			       (string-match "\\(\\.\\)" (buffer-name))))))
    (setq grepstring 
	  (concat "grep " opts "  \"" sstring "\" " filestr))
    ;; evaluate the grep
    (grep grepstring)))


(defun mhs-grep-these-prompt (ps) "\
   *grep all files with this extension with value entered by user"
  (interactive "sWhat do you want to grep for: ")
    (mhs-grep 't nil ps))


(defun mhs-nocase-grep-these-prompt (ps) "\
   *grep all files with this extension with value entered by user"
  (interactive "sWhat do you want to grep for: ")
  (mhs-grep 't "i" ps))

(defun mhs-grep-these( &optional o) "\
   *Grep for region in all files with same extension as buffer"
  (interactive)
  (mhs-grep 't o))

(defun mhs-nocase-grep-these() "\
   *Grep for region in all files with same extension as buffer case insensitive"
  (interactive)
  (mhs-grep-these "i"))


(defun mhs-grep-thisfile (&optional o) "\
  *Grep for region in this file only (case sensitive when options not supplied)"
  (interactive)
  (mhs-grep nil o))

(defun mhs-nocase-grep-thisfile () "\
 *Grep for region in this file case insensitive"
  (interactive)
  (mhs-grep-thisfile "i"))


(provide 'mhs-grep)
  
