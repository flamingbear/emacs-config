;;; MHS-BBDB.EL --- Quick place to load BBDB stuff that I want on multiple machines.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 04 Oct 2011
;; Version: 1.0
;; Keywords:


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
  (if running-macos
      (bbdb-initialize 'gnus 'message)
    (bbdb-initialize 'gnus 'message 'sc)))

;; This 'sc = supercite and causes beep in loading this file on macosx

(provide 'mhs-bbdb)

;;; MHS-BBDB.EL ends here
