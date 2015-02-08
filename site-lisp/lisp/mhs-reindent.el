;;; MHS-REINDENT.EL --- reindents code

;; Copyright (C) 2000 

;; Author:  <msavoie@sagent.com>
;; Maintainer:  <msavoie@sagent.com>
;; Created: 05 Sep 2000
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
;; program's author (send electronic mail to <msavoie@sagent.com>) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.

;; LCD Archive Entry:
;; mhs-reindent||<msavoie@sagent.com>
;; |reindents code
;; |$Date: 2011-09-18 10:23:58 -0600 (Sun, 18 Sep 2011) $|$Revision: 19387 $|~/packages/mhs-reindent.el

;;; Commentary:

;;; Change log:
;; $Log: not supported by cvs2svn $

;;; Code:



(defun mhs-reindent ()
  "*Reindents the entire loaded file."
  (interactive)
  (my-c-mode-almost-common-hook)
  (mark-whole-buffer)
  (call-interactively 'indent-region)
  (save-buffer))

(provide 'mhs-reindent)
;;; MHS-REINDENT.EL ends here
