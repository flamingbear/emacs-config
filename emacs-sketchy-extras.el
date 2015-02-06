;;; EMACS-SKETCHY-EXTRAS.EL --- Just a place to put things that work only with the very latest emacs so that it can be ignored when needed.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 14 Jan 2011
;; Version: 1.0
;; Keywords:




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

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


;; Try company-mode instead of auto-complete
;;--------------------------
(add-hook 'after-init-hook 'global-company-mode)
;; (company-quickhelp-mode 1)



;; Yasnippet
;;-----------
(when (try-require 'yasnippet) ;; not yasnippet-bundle
  (yas-global-mode 1))

;;; Chrome editing.
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))


;;; EMACS-SKETCHY-EXTRAS.EL ends here
