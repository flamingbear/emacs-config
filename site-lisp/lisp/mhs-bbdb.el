;;; MHS-BBDB.EL --- Quick place to load BBDB stuff that I want on multiple machines.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 04 Oct 2011
;; Version: 1.0
;; Keywords:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBDB

;; Location on macintosh machines.
(use-package bbdb
  :defer 3
  :ensure t
  :config
  (setq bbdb-use-pop-up nil)
  (setq bbdb-file (expand-file-name "private/.bbdb.gpg" user-emacs-directory))
  (if running-macos
      (bbdb-initialize 'gnus 'message)
    (bbdb-initialize 'gnus 'message 'sc)))

;; This 'sc = supercite and causes beep in loading this file on macosx

(provide 'mhs-bbdb)

;;; MHS-BBDB.EL ends here
