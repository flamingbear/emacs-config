;;; EMACS-SKETCHY-EXTRAS.EL --- Just a place to put things that work only with the very latest emacs so that it can be ignored when needed.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 14 Jan 2011
;; Version: 1.0
;; Keywords:


;;; Code:

(defconst emacs-sketchy-extras-version (substring "$Revision: 19610 $" 11 -2)
  "$Id: emacs-sketchy-extras.el 19610 2011-10-20 20:59:03Z savoie $
Report bugs to: Matt Savoie <emacs@flamingbear.com>")


;; See if you can run orgmode from this computer.
(defvar mhs-org-mode-directory (expand-file-name "~savoie/Dropbox/orgs/")
  "Location of my .org mode files" )

(when (and (file-accessible-directory-p mhs-org-mode-directory)
           (try-require 'mhs-org-mode))
  (set-variable 'comment-start 'nil)
  (setq org-agenda-custom-commands
        '(("Q" . "Custom queries") ;; gives label to "Q"
          ("Qa" "Archive search" search ""
           ((org-agenda-files (file-expand-wildcards "~/Dropbox/orgs/*.org_archive"))))
          ("Qb" "Projects and Archive" search ""
           ((org-agenda-text-search-extra-files (file-expand-wildcards "~/Dropbox/orgs/*.org_archive"))))
          ;; searches both projects and archive directories
          ("QA" "Archive tags search" org-tags-view ""
           ((org-agenda-files (file-expand-wildcards "~/Dropbox/orgs/*.org_archive"))))))
  "mhs-org mode loaded"  )


;; New Better Autocomplete?
;;--------------------------

(when (try-require 'auto-complete-config)
  (progn (add-to-list 'ac-dictionary-directories (concat mhs-external-lisp-dir "ac-dict"))
         (add-to-list 'ac-modes 'idlwave-mode)
         (ac-config-default)))


;; Yasnippet
;;-----------
(add-to-list 'load-path
             (concat emacs-top "external-lisp-files/yasnippet"))

(when (try-require 'yasnippet) ;; not yasnippet-bundle
  (progn (yas/initialize)
         (setq yas/root-directory (concat emacs-top "external-lisp-files/yasnippet/snippets"))
         (yas/load-directory yas/root-directory)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Secure Shell for Remote access
;; To use:
;;  /[<machine name>]/path/to/file
;;  /[<machine name>]~/expanded/homedir/file
;; /[<machine name>].emacs
;; or assume home directory
(when (try-require 'tramp)
  (setq tramp-default-method "scp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Use the gnus news reader
(defvar mhs-bleeding-gnus (concat emacs-top "external-lisp-files/bleeding-gnus/lisp")
  "Location of the git repository of gnus for testing")
(if (file-accessible-directory-p mhs-bleeding-gnus)
    (progn (add-to-list 'load-path mhs-bleeding-gnus 'nil)
           (require 'gnus-load))
  (try-require 'gnus))


;; Proper gnus reader
(if (try-require 'gnus-w3m)
    (setq mm-text-html-renderer 'gnus-w3m)
   (progn                                ;macosx mainly
     (setq mm-text-html-renderer 'gnus-article-html)
     (setq mm-inline-text-html-with-images 't) ))

;; R-language support.
(try-require 'ess-site)

;; Ctypes support
(when (try-require 'ctypes)
  (setq ctypes-file-name "~savoie/.ctypes")
  (setq ctypes-write-types-at-exit t)
  (ctypes-read-file nil nil t t)
  (ctypes-auto-parse-mode 1))


;;; Chrome editing.
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))



;;; EMACS-SKETCHY-EXTRAS.EL ends here
