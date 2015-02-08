;;; MHS-DBLSTUFF.EL --- smart inserting of double characters.

;; Copyright (C) 1997 Matthew H. Savoie

;; Author: Matthew H. Savoie <savoie@fsl.noaa.gov>
;; Maintainer: Matthew H. Savoie <savoie@fsl.noaa.gov>
;; Created: 01 May 1997
;; Version: 1.0
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <savoie@fsl.noaa.gov>) or
;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;; MA 02139, USA.

;; LCD Archive Entry:
;; mhs-dblstuff|Matthew H. Savoie|<savoie@fsl.noaa.gov>
;; |smart inserting of double characters.
;; |$Date: 2011-09-18 10:23:58 -0600 (Sun, 18 Sep 2011) $|$Revision: 19387 $|~/packages/mhs-dblstuff.el

;;; Commentary:

;; to Install  put (require 'mhs-dblstuff) in your .emacs 
;; bind to a useful key sequence 
;; (global-set-key [(control c) (d)] 'mhs-dblstuff)

;; This package should "do the right thing" If the region is active,
;; it places the characters around the region.  If not, it places them
;; at the point.


;;     Many of the ideas for this came directly from t-match.el by 
;;;;; Author:          Tom Kraljevic
;;;;; Email:           tomk@crhc.uiuc.edu


;;; Change log:
;; $Log: not supported by cvs2svn $
;; Revision 1.3  1997/09/18 14:53:52  savoie
;; last before emacs 20.1
;;
;; Revision 1.2  1997/05/01 22:03:12  savoie
;; first version that works.  I've been wanting this for a while.
;; Coolies
;;
;; Revision 1.1  1997/05/01 20:31:01  savoie
;; Initial revision
;;

;;; Code:

(defconst mhs-dblstuff-version (substring "$Revision: 19387 $" 11 -2)
  "$Id: mhs-dblstuff.el 19387 2011-09-18 16:23:58Z savoie $
Report bugs to: Matthew H. Savoie <savoie@fsl.noaa.gov>")


;;; global variables.

(defvar lr-pairs '( ("(" ")") ("{" "}") ("[" "]") ("<" ">")))


;;;;; FUNCTIONS


(defun mhs-dblstuff-get-right (lr-pair)
  (car (cdr lr-pair)))

;; remember (a b) = (a . (b . nil))

(defun mhs-dblstuff-get-left (lr-pair)
  (car lr-pair))


(defun mhs-get-match-pair (tok)
  ;; find out which pair matches the token and return the pair,
  ;; or...return a made up pair of the character twice.
  (let ( (rest-match nil)
	 (done nil)
	 (left nil)
	 (right nil)
	 (lr-pair nil))
    (setq rest-match lr-pairs)
    (while (and (not (null rest-match)) (not done))
      (setq lr-pair (car rest-match))
      (setq rest-match (cdr rest-match))
      (setq left (mhs-dblstuff-get-left lr-pair))
      (setq right (mhs-dblstuff-get-right lr-pair))
      (if 
	  (or (string-equal left (char-to-string tok))
	      (string-equal right (char-to-string tok)))
	  (setq done t)))
    (if (not done) 
	(setq lr-pair (list (char-to-string tok) (char-to-string tok))))
    lr-pair))


(defun mhs-dblstuff (tok)
  "*insert doubled character based on next character input and the region hilighted" 
  (interactive "c")
  (save-excursion
    (let ((start (if mark-active (min (point) (mark)) (point)))
	  (finish (if mark-active (max (point) (mark)) (point)))
	  (match-pair (mhs-get-match-pair tok)))
    (progn 
      (goto-char start)
      (insert (mhs-dblstuff-get-left match-pair))
      (goto-char (+ finish 1))
      (insert (mhs-dblstuff-get-right match-pair))))))

(provide 'mhs-dblstuff)

;;; MHS-DBLSTUFF.EL ends here
