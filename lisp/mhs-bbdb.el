;;; MHS-BBDB.EL --- Quick place to load BBDB stuff that I want on multiple machines.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 04 Oct 2011
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
;; mhs-bbdb|Matt Savoie|<savoie@nsidc.org>
;; |Quick place to load BBDB stuff that I want on multiple machines.
;; |$Date$|$Revision: 19517 $|~/packages/mhs-bbdb.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-bbdb-version (substring "$Revision: 19517 $" 11 -2)
  "$Id: mhs-bbdb.el 19517 2011-10-04 15:19:39Z savoie $
Report bugs to: Matt Savoie <savoie@nsidc.org>")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBDB
;;  loads only if it finds the directories needed.
(defvar mhs-bbdb-dir (expand-file-name (concat emacs-top '"local/share/emacs/site-lisp/bbdb"))
  "Location of the bbdb install")

;; Location on macintosh machines.
(when running-macos 
  (setq mhs-bbdb-dir (expand-file-name (concat mhs-external-lisp-dir '"bbdb/lisp"))))


(when (file-accessible-directory-p mhs-bbdb-dir )
  (add-to-list 'load-path mhs-bbdb-dir))
(when (try-require 'bbdb)
  (bbdb-initialize 'gnus 'message 'sc))
;; This 'sc = supercite and causes beep in loading this file on macosx

(provide 'mhs-bbdb)

;;; MHS-BBDB.EL ends here
