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

;; This was all "cleaned up" with chatgpt on 2024-02-06 for fun because I wrote
;; this in 1997 without really know much about lisp.

(defun mhs-get-right (lr-pair)
  "Return the right element of LR-PAIR."
  (cadr lr-pair))


(defun mhs-get-left (lr-pair)
  "Return the left element of LR-PAIR."
  (car lr-pair))

(defun mhs-get-match-pair (tok)
  "Given input charcter TOK, return a matching pair.
If there is no match, just duplicate the input TOK."
  (let* ((lr-pairs '(("[" "]") ("(" ")") ("{" "}") ("<" ">")))
         (tok-str (char-to-string tok))
         (matching-pair (cl-find-if (lambda (pair)
                                      (or (string-equal tok-str (car pair))
                                          (string-equal tok-str (cadr pair))))
                                    lr-pairs)))
    (or matching-pair
        (list tok-str tok-str))))

(defun mhs-dblstuff (tok)
  "Insert next character TOK and its match around region highlighted."
  (interactive "c")
  (let ((start (if (region-active-p) (region-beginning) (point)))
        (finish (if (region-active-p) (region-end) (point)))
        (match-pair (mhs-get-match-pair tok)))
    (save-excursion
      (goto-char start)
      (insert (mhs-get-left match-pair))
      (goto-char (1+ finish))
      (insert (mhs-get-right match-pair)))))


(provide 'mhs-dblstuff)

;;; MHS-DBLSTUFF.EL ends here
