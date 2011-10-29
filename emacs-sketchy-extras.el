;;; EMACS-SKETCHY-EXTRAS.EL --- Just a place to put things that work only with the very latest emacs so that it can be ignored when needed.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 14 Jan 2011
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
;; |Matt Savoie|<savoie@nsidc.org>
;; |Just a place to put things that work only with the very latest emacs so that it can be ignored when needed.
;; |$Date: 2011-10-20 14:59:03 -0600 (Thu, 20 Oct 2011) $|$Revision: 19610 $|~/packages/emacs-sketchy-extras.el

;;; Commentary:


;;; Code:

(defconst emacs-sketchy-extras-version (substring "$Revision: 19610 $" 11 -2)
  "$Id: emacs-sketchy-extras.el 19610 2011-10-20 20:59:03Z savoie $
Report bugs to: Matt Savoie <savoie@nsidc.org>")


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
(try-require 'gnus)
(try-require 'mhs-mail)

(setq mm-text-html-renderer 'w3m)

;; R-language support.  
(try-require 'ess-site)

;; Ctypes support
(when (try-require 'ctypes)
  (setq ctypes-file-name "~savoie/.ctypes")
  (setq ctypes-write-types-at-exit t)

  (ctypes-read-file nil nil t t)
  (ctypes-auto-parse-mode 1))





;;; EMACS-SKETCHY-EXTRAS.EL ends here
