;;; MHS-CLISP-STUFF.EL --- Loads up common lisp environment on machines where it exists.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 01 Oct 2011
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
;; program's author (send electronic mail to <savoie@nsidc.org>) or from the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; mhs-clisp-stuff|Matt Savoie|<savoie@nsidc.org>
;; |Loads up common lisp environment on machines where it exists.
;; |$Date$|$Revision: 19511 $|~/packages/mhs-clisp-stuff.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-clisp-stuff-version (substring "$Revision: 19511 $" 11 -2)
  "$Id: mhs-clisp-stuff.el 19511 2011-10-03 15:46:13Z savoie $

Report bugs to: Matt Savoie <savoie@nsidc.org>")

(defvar mhs-inferiorlisp-system "/usr/local/bin/sbcl")
(defvar mhs-slime-dir (concat mhs-external-lisp-dir "slime/"))

(when (file-accessible-directory-p mhs-slime-dir)
    (add-to-list 'load-path mhs-slime-dir)  ; your SLIME directory
    (setq inferior-lisp-program mhs-inferiorlisp-system) ; your Lisp system
    (try-require 'slime)
    (slime-setup))

(provide 'mhs-clisp-stuff)


;;; MHS-CLISP-STUFF.EL ends here
