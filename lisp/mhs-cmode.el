;;; MHS-CMODE --- customization package for c/cpp/java coding

;; Copyright (C) 1997 Matthew H. Savoie

;; Author: Matthew H. Savoie <savoie@fsl.noaa.gov>
;; Maintainer: Matthew H. Savoie <savoie@fsl.noaa.gov>
;; Created: 26 Mar 1997
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
;; |Matthew H. Savoie|<savoie@fsl.noaa.gov>
;; |customization package for c/cpp/java coding
;; |$Date: 2011-09-18 10:23:58 -0600 (Sun, 18 Sep 2011) $|$Revision: 19387 $|~/packages/.emacs_mycmode

;;; Commentary:

;;; Change log:
;; $Log: not supported by cvs2svn $
;; Revision 1.1.1.1  2002/02/28 18:00:54  savoie
;; imported sources
;;
;; Revision 1.3  1997/09/18 14:53:43  savoie
;; last before emacs 20.1
;;
;; Revision 1.2  1997/03/26 18:16:50  savoie
;; original version before I screw it up.
;;
;; Revision 1.1  1997/03/26 18:11:15  savoie
;; Initial revision
;;

;;; Code:

(defconst -version (substring "$Revision: 19387 $" 11 -2)
  "$Id: mhs-cmode.el 19387 2011-09-18 16:23:58Z savoie $

Report bugs to: Matthew H. Savoie <mattie@innocent.com>")


(setq c-style-variables-are-local-p t)
(setq c-basic-offset 4)
(setq c-hanging-comment-ender-p nil)

(setq auto-mode-alist
     (append
      '(("\\.C$"    . c++-mode)
	 ("\\.H$"    . c++-mode)
	 ("\\.cc$"   . c++-mode)
	 ("\\.hh$"   . c++-mode)
	 ("\\.c$"    . c-mode)
	 ("\\.h$"    . c-mode)
;;	 ("\\.m$"    . objc-mode)
	 ("\\.java$" . java-mode)
	 ) auto-mode-alist))

;; Here's a sample .emacs file that might help you along the way.  Just
;; copy this region and paste it into your .emacs file.  You may want to
;; change some of the actual values.
(defconst my-java-style
  '( "java"
     (c-basic-offset . 4)
     (c-hanging-braces-alist  . ((substatement-open after)
				 (defun-open after)
				 (defun-close after)
				 (class-open after)
				 (class-close after)
				 (inline-open after)
				 (inline-close after)
				 (block-open after)
				 (block-close after)
				 (statement-case-open after)
				 (extern-lang-open after)
				 (extern-lang-close after)
				 (brace-list-open after)
				 (brace-list-close after)
				 (brace-list-intro after)
				 (brace-list-entry after)))
     (topmost-intro . 0)
     (topmost-intro-cont . 0)
     ) "My Java Programming Style")



(defconst my-c-style
  '((c-basic-offset              . 4)
    (c-tab-always-indent         . t)
    (c-hanging-comment-ender-p   . nil)
    (c-hanging-comment-starter-p . nil)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((substatement-open after)
				    (block-close . c-snug-do-while)
				    (extern-lang-open after)
				    (brace-list-open after)))
    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))
    (c-cleanup-list             . (scope-operator
				   empty-defun-braces
				   defun-close-semi
				   list-close-comma))
    (c-offsets-alist            . ((arglist-close     . c-lineup-arglist)
				   (substatement-open . 0)
				   (statement . 0)
				   (case-label        . +)
				   (access-label      . -)
				   (inline-open       . 0)
				   (defun-block-intro . +)
				   (statement-block-intro . +)
				   (topmost-intro     . 0)
				   (block-open        . 0)
				   (knr-argdecl-intro . +)))
    (c-echo-syntactic-information-p    . t)
    (c-indent-comments-syntactically-p . t)
    )
  "My C Programming Style"  )



;(defun my-ctypes-load-hook ()
;  (ctypes-read-file nil nil t t)
;  (ctypes-all-buffers)
;  (ctypes-auto-parse-mode 1))
;(add-hook 'ctypes-load-hook 'my-ctypes-load-hook)

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun my-c-mode-almost-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-set-style "k&r")
  (c-add-style "PERSONAL" my-c-style t)
  ;; other customizations
  ;;---------------------
  (setq tab-width 4 indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  (local-set-key (read-kbd-macro "M-RET") 'mhs-insert-newline)
  (local-set-key [(control meta return)] 'mhs-insert-newline-ender)
  (local-set-key [M-up]   '(lambda() (interactive) (if-jump-jump 'backward)))
  (local-set-key [M-down] '(lambda() (interactive) (if-jump-jump 'forward)))

  ;; keybindings for C, C++, and Objective-C.  We can put these in
  ;; c-mode-map because c++-mode-map and objc-mode-map inherit it
  (define-key c-mode-map "\C-m" 'newline-and-indent))

(defun my-c++-mode-almost-common-hook ()
  (local-set-key (read-kbd-macro "C-c u") 'mhs-underline-c++-comment))

(defun my-java-mode-hook ()
  (c-toggle-auto-hungry-state 1)
  (c-add-style "myjava" my-java-style t))




;; the following only works in Emacs 19
;; Emacs 18ers can use (setq c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-hook 'my-c-mode-almost-common-hook)
(add-hook 'c++-mode-hook 'my-c-mode-almost-common-hook)
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-almost-common-hook)

(provide 'mhs-cmode)

;;; mhs-cmode.el ends here

