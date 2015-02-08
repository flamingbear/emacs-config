;;; MHS-BBDB.EL --- Quick place to load BBDB stuff that I want on multiple machines.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
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
Report bugs to: Matt Savoie <emacs@flamingbear.com>")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBDB

;; Location on macintosh machines.

(when (try-require 'bbdb)
  (if running-macos
      (bbdb-initialize 'gnus 'message)
    (bbdb-initialize 'gnus 'message 'sc)))

;; This 'sc = supercite and causes beep in loading this file on macosx

(provide 'mhs-bbdb)

;;; MHS-BBDB.EL ends here
