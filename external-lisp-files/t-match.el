;; $Log: not supported by cvs2svn $
;; Revision 1.1.1.1  2002/02/28 18:00:54  savoie
;; imported sources
;;
;; Revision 0.0  1996/11/18 18:03:53  savoie
;; *** empty log message ***
;;
;;
;; Purpose of this package:
;;     Match parentheses and any other arbitrary strings.
;;
;; Installation instructions:
;;     (autoload 't-match-move-point-to-matching "t-match" nil t nil)
;;     (define-key global-map "\M-p" 't-match-move-point-to-matching)
;;
;; Usage instructions:
;;     M-p moves point to matching token (or display error message
;;        if none exists).  If the move is successful, the old point
;;        location is pushed on the mark stack.
;;
;; Known bugs:
;;     none.
;;
;; LCD Archive Entry:
;; t-match|Tom Kraljevic|tomk@crhc.uiuc.edu|
;; Match parens and other arbitrary strings.|
;; 0-Oct-1994|1.3|~/misc/t-match.el.Z|
;;
;;;;;
;;;;; $Header: /tmp_mnt/FILES/mhs/mhs-lisp/lisp/t-match.el,v 1.2 2008-05-08 19:09:42 savoie Exp $
;;;;;
;;;;; Tom's match.el (t-match.el)
;;;;;
;;;;; Author:          Tom Kraljevic
;;;;; Email:           tomk@crhc.uiuc.edu
;;;;;
;;;;; Creation date:   8 May 1994
;;;;;
;;;;; Package:         t-match.el v1.3
;;;;;
;;;;; Motivation:      I wanted some elisp programming experience, and I was
;;;;;                  frustrated with editing .m4 files.
;;;;;
;;;;; Purpose:         Match (), {}, [], /* */, `', and any other combination
;;;;;
;;;;; Compatibility:   This package was developed on FSF Emacs 19.19 and may or
;;;;;                  may not work with different versions of Emacs.
;;;;;
;;;;; Method:          Searching is done by a non-regexp version of looking-at.
;;;;;                  No syntax tables are used.
;;;;;

;;;;;
;;;;; User-callable functions:
;;;;;
;;;;; t-match-move-point-to-matching
;;;;;     Attempt to move the point to the matching brace, paren, etc.
;;;;;     Left/right matching tokens may be of arbitrary length and
;;;;;     are read from t-match-list.  If a successful match is found, 
;;;;;     move point to the first character of the matched token, and
;;;;;     push the old point on the mark stack.  If a successful match
;;;;;     is not found, an error message will display.  A global variable
;;;;;     controls case-sensitivity.
;;;;;

;;;;;
;;;;; User-defined global variables 
;;;;;
;;;;; t-match-list
;;;;;     List containing pairs of left/right tokens.  Tokens may be
;;;;;     arbitrary length.  Note that no token should be used twice in
;;;;;     the list for obvious reasons.
;;;;;
;;;;; t-match-ignore-case
;;;;;     Whether or not to ignore case in the searches.
;;;;;

;;;;;
;;;;; Tom's suggested configuration:
;;;;;
;;;;; (autoload 't-match-move-point-to-matching "t-match" nil t nil)
;;;;; (define-key global-map "\M-p" 't-match-move-point-to-matching)
;;;;;

;;;;;
;;;;; Misc. comments:  It's not pretty Lisp code, but I don't care!
;;;;;
;;;;;                  There is no differentiation between tokens in
;;;;;                     a string or not in a string.
;;;;;
;;;;; Possible nomenclature nit:  I use the term `token' slightly loosely, 
;;;;; in that one of my `tokens' doesn't necessarily need to be surrounded
;;;;; by whitespace or reserved characters.  Perhaps the word "string" 
;;;;; would be more appropriate.  Oh well.
;;;;;

;;;;;
;;;;; Known bugs:
;;;;;   none.
;;;;;

;; Go ahead and try it out:
;; Begin
;; begin
;; BEGIN
;; End
;; END
;; end


;;;; GLOBALS

(defvar t-match-list '( ("/*" "*/") ("(" ")") ("{" "}") ("[" "]") ("`" "'")
			("\\begin" "\\end") ("begin" "end") ("Begin" "End") 
			("BEGIN" "END") ("<%" "%>") ("<" ">")))

(defvar t-match-ignore-case nil)      ; don't ignore case


;;;; FUNCTIONS


(defun t-match-get-left-string (left-right-pair)
  (car left-right-pair))

(defun t-match-get-right-string (left-right-pair)
  (car (cdr left-right-pair)))

(defun t-match-left-token-p (left-right-pair)
  (equal 'left (car (cdr (cdr left-right-pair)))))

;;;
;;; Just like looking-at, but with a string instead of a regexp.
;;;
(defun t-match-looking-at-string (string)
  (if (> (+ (point) (length string)) (point-max))
      nil
    (string-equal (buffer-substring (point) 
				    (+ (point) 
				       (length string)))
		  string)))

(defun t-match-point-on-matchable-char-p ()
;;;
;;;  Return value:  If point is over one of the tokens in the match list, then
;;;                 return-value == (LEFT-TOKEN RIGHT-TOKEN 'left/'right) where
;;;                 'left/'right == 'left if point is over the left token and 
;;;                 'left/'right == 'right if (you guessed it) point is over the
;;;                 right token.
;;;
;;;                 If point is over a nonmatchable token, return nil.
;;;
  (let ((rest-match-list nil)
	(done nil)
	(return-val nil)
	(left-right-pair nil)
	(left-string nil)
	(right-string nil))
    
    (setq rest-match-list t-match-list)
    (while (and (not (null rest-match-list)) (not done))
      (setq left-right-pair (car rest-match-list))
      (setq left-string (t-match-get-left-string left-right-pair))
      (setq right-string (t-match-get-right-string left-right-pair))

      (save-excursion 

	(if (t-match-looking-at-string left-string)
	    (progn (setq return-val (list left-string right-string 'left))
		   (setq done t))
	  (let ((save-point (point))
		(found-left-token nil))
	    (goto-char (min (+ save-point (length left-string)) (point-max)))
	    (setq found-left-token (search-backward left-string nil t))
	    (if (and found-left-token (> (+ (point) (length left-string)) 
					 save-point))
		(progn (setq return-val (list left-string right-string 'left))
		       (setq done t)))
	    (goto-char save-point)))

	(if (t-match-looking-at-string right-string)
	    (progn (setq return-val (list left-string right-string 'right))
		   (setq done t))
	  (let ((save-point (point))
		(found-right-token nil))
	    (goto-char (min (+ save-point (length right-string)) (point-max)))
	    (setq found-right-token (search-backward right-string nil t))
	    (if (and found-right-token (> (+ (point) (length right-string)) 
					  save-point))
		(progn (setq return-val (list left-string right-string 'right))
		       (setq done t))))))

      (setq rest-match-list (cdr rest-match-list)))

    return-val))


(defun t-match-move-point-to-matching ()
;;;
;;;  Returns:  nothing (meaningful)
;;;            
;;;  Attempt to perform the match and move point.  If point is moved,
;;;  the old value of point is pushed on the mark stack.
;;;
;;;  If the match cannot be performed, an error is signalled via 
;;;  message().
;;;
  "Move point to matching (with `token' under point) token."
  (interactive)
  (let ((init-case-fold-search case-fold-search)
	(point-info nil)
	(match-count nil)               ; match-count is incremented
					; by 1 for
					; each right token hit,
					; and decremented by one
					; for each left token hit.

	(next-t-match-left-token-pos nil)       ; Location of temporary point
					; in buffer.
	(next-right-token-pos nil)      ;     ""
	(initial-save-point (point))    ; initial value of point
	(save-point nil)
	(final-point (point))
	(left-token nil)
	(right-token nil)
	(found-left nil)
	(found-right nil))    
    (setq case-fold-search t-match-ignore-case)
    (setq point-info (t-match-point-on-matchable-char-p))
    (if point-info
	(progn
	  (setq left-token (t-match-get-left-string point-info))
	  (setq right-token (t-match-get-right-string point-info))

	  (save-excursion
	    (if (t-match-left-token-p point-info)      ; cursor under left token

		;; LEFT TOKEN UNDER POINT

		(progn
		  (setq match-count -1) ; start at -1 for left token
		  (if (t-match-looking-at-string left-token)
		      (search-forward left-token nil t))
					; do this so that we don't count
					; the token under point twice.

		  (while (not (equal match-count 0))
		    (setq save-point (point))
		    (setq found-left (search-forward left-token nil t))
		    (setq next-left-token-pos (point))
		    (goto-char save-point)
		    (setq found-right (search-forward right-token nil t))
		    (setq next-right-token-pos (point))
		    (goto-char save-point)
		    
		    (if (and found-left found-right)
			(if (< found-left found-right)
			    (progn
			      (goto-char next-left-token-pos)
			      (setq match-count (- match-count 1)))
					; subtract 1 from match
					; count because of the left token.
			  (progn 
			    (goto-char next-right-token-pos)
			    (setq match-count (+ match-count 1))))
					; add 1 to match count because
					; of the right token.
		      (if (not found-right)
					; Not enough right tokens in 
					; the file.
					; Error condition.
			  (progn
			    (message (format "Not enough right %s." 
					     right-token))
			    (setq match-count 0))
					; `fake' being done.  Actually,
					; we did nothing.

			(progn
			  (goto-char next-right-token-pos)
			  (setq match-count (+ match-count 1))))))
					; add 1 to match count because
					; of the right token.

		  (if found-right
		      (progn
			(search-backward right-token nil t)
			(setq final-point (point)))
		    (progn 
		      (setq final-point initial-save-point))))

	      ;; RIGHT TOKEN UNDER POINT

	      (progn
		(setq match-count 1) ; start at 1 for left token
		(if (not (t-match-looking-at-string right-token))
		    (progn 
		      (goto-char (+ (point) (length right-token)))
		      (search-backward right-token nil t)))
					; do this so that we don't count
					; the token under point twice.

		(while (not (equal match-count 0))
		  (setq save-point (point))
		  (setq found-left (search-backward left-token nil t))
		  (setq next-left-token-pos (point))
		  (goto-char save-point)
		  (setq found-right (search-backward right-token nil t))
		  (setq next-right-token-pos (point))
		  (goto-char save-point)
		    
		  (if (and found-left found-right)
		      (if (> found-left found-right)
			  (progn
			    (goto-char next-left-token-pos)
			    (setq match-count (- match-count 1)))
					; subtract 1 from match
					; count because of the left token.
			(progn 
			  (goto-char next-right-token-pos)
			  (setq match-count (+ match-count 1))))
					; add 1 to match count because
					; of the right token.
		    (if (not found-left)
					; Not enough left tokens in 
					; the file.
					; Error condition.
			(progn
			  (message (format "Not enough left %s." 
					   left-token))
			  (setq match-count 0))
					; `fake' being done.  Actually,
					; we did nothing.

		      (progn
			(goto-char next-left-token-pos)
			(setq match-count (- match-count 1))))))
					; sub 1 to match count because
					; of the left token.

		(if (not found-left)
		    (setq final-point initial-save-point)
		  (setq final-point (point))))))

	  ;; End of save-excursion.  Now change the point for real.

	  (if (not (equal initial-save-point final-point))
	      (progn (goto-char initial-save-point)
		     (push-mark)
		     (goto-char final-point))))
    (message "Token under cursor not matchable (not in t-match-list)."))
    (setq case-fold-search init-case-fold-search)))


