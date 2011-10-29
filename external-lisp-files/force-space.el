;;; @(#) force-space.el --- forces spaces around certain
;; regular expressions.

;; Copyright (C) 2000-2005
;;           Kevin A. Burton (burton@apache.org), All rights reserved.
;;
;; Created:     2000
;; RCS version: $Revision: 19505 $
;; Date:        $Date: 2011-10-01 15:15:40 -0600 (Sat, 01 Oct 2011) $
;; Keywords:    tools

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  This package provides a set of functions, compatible with all major and
;;  minor editing modes, for forcing spaces around certain characters.  This was
;;  written for the Java programming language but should work for almost any
;;  language based around C (C, C++, python, perl, etc).  If it doesn't work for
;;  you target language please feel free to add this feature.
;;
;;  It is really only desirable for c style major modes (although you could
;;  easily modify it to work with Lisp, etc). It might be necessary to adapt
;;  this in the future to modify itself based on the current mode.  A good
;;  example of this would be to ignore '(' and ')' in Lisp mode since this is
;;  *very* integral to the language and this type of format for Lisp would be
;;  ugly.
;;
;;  Examples of strings that are forced:
;;
;;      - "("     ->    "( "
;;      - ")"     ->    " )"
;;      - "="     ->    " = "
;;      - "=="    ->    " == "
;;      - "["     ->    "[ "
;;      - "]"     ->    " ]"
;;


;;; Code:

(defun force-space()
  "Replace all 'space desired' characters in the current buffer.

The following tests should pass:

--

    buff.append(getStrings().get(key));

    buff.append( getStrings().get( key ) );

"
  (interactive)


  ;;it is important to start of at the begining of the buffer
  (let(start end)
    (save-excursion
      (goto-char (point-at-bol))
      (setq start (point))

      (goto-char (point-at-eol))
      (setq end (point))

      
      (save-restriction

        (narrow-to-region start end)

        ;;spaces around parens ( and ) .
        (force-space-regexp "\\([(]\\)[^() ]" "( ")
        (force-space-regexp "[^() ]\\([)]\\)" " )")
        (force-space-regexp "\\())\\)" ") )")
        
        ;;spaces around brakets { and } .
        (force-space-regexp "[^{} ]\\([{]\\)" " {")
        (force-space-regexp "[^{} ]\\([}]\\)" " }")

      
        ;; change ',' into ', '
        (force-space-regexp "\\([,]\\)[^, ]" ", ")

        ;; spaces around [ and ]
        (force-space-regexp "\\([\[]\\)[^\] ]" "[ " )
        (force-space-regexp "[^\[ ]\\([\]]\\)" " ]" )


;;         ;;spaces around =
;;         (force-space-regexp "[^ =]\\(=\\)" " =")
;;         (force-space-regexp "\\(=\\)[^ =]" "= ")

        ;;spaces around ==
        (force-space-regexp "[^ ]\\(==\\)" " ==")
        (force-space-regexp "\\(==\\)[^ ]" "== ")))))



(defun force-space-regexp(regexp replacement)
  "Perform replacement.  Regexp must have at least one valid regexp."
  
  (save-excursion
    (goto-char (point-min))
    
    (while (re-search-forward regexp nil t)
        
      (let((match-begin (match-beginning 1))
           (match-end (match-end 1)))
        
        (delete-region match-begin match-end)
        
        (goto-char match-begin)
        
        (insert replacement)))))

(provide 'force-space)

;;; force-space.el ends here
