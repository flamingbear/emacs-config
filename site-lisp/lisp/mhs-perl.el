;;; MHS-PERL.EL --- changes to the perl package in here

;; Copyright (C) 1997 Matthew H. Savoie

;; Author: Matthew H. Savoie <savoie@fsl.noaa.gov>
;; Maintainer: Matthew H. Savoie <savoie@fsl.noaa.gov>
;; Created: 22 Apr 1997
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
;; mhs-perl|Matthew H. Savoie|<savoie@fsl.noaa.gov>
;; |changes to the perl package in here
;; |$Date: 2011-10-10 08:28:58 -0600 (Mon, 10 Oct 2011) $|$Revision: 19561 $|~/packages/mhs-perl.el

;;; Commentary:

;;; Change log:
;; $Log: not supported by cvs2svn $
;; Revision 1.1.1.1  2002/02/28 18:00:54  savoie
;; imported sources
;;
;; Revision 1.2  1997/09/18 14:54:03  savoie
;; last before emacs 20.1
;;
;; Revision 1.1  1997/04/22 23:06:13  savoie
;; Initial revision
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use cperl-mode by default
(use-package cperl-mode
  :ensure t
  :config
  (defalias 'perl-mode 'cperl-mode)


  (defun perltidy-region ()
    "Run perltidy on the current region."
    (interactive)
    (save-excursion
      (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

  (defun perltidy-defun ()
    "Run perltidy on the current defun."
    (interactive)
    (save-excursion
      (mark-defun) (perltidy-region)))
)

(provide 'mhs-perl)
;;; MHS-PERL.EL ends here
