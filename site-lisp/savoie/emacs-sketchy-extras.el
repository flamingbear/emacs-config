;;; EMACS-SKETCHY-EXTRAS.EL --- Just a place to put things that work only with the very latest emacs so that it can be ignored when needed.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 14 Jan 2011
;; Version: 1.0
;; Keywords:



;; See if you can run orgmode from this computer.
(use-package org
  :config
  (setq org-agenda-files "~/Dropbox/orgs/org-agenda-files")
  (setq org-clock-persist (quote history))
  (setq org-clock-persist-file "~/Dropbox/orgs/org-clock-save.el")
  (setq org-log-done (quote note))
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5))))
  (setq org-remember-default-headline "TASKS.org")
  (setq org-remember-templates
        '(("todo" 116 "* TODO %? %u %a" nil nil nil)
          ("note" 110 "* %?" nil nil nil)
          ("Url" 117 "* %^{Title} Source: %u, %c	%i" nil nil nil)))
  (setq org-tag-alist   '(("daac" . 100)
                          ("programmer" . 112)
                          ("erik" . 101)
                          ("management" . 109)
                          ("services" . 115)
                          ("masie" . 105)
                          ("annual_review" . 97)))
  (setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "DELEGATED" "CANCELLED")))

  (defvar mhs-org-mode-directory (expand-file-name "~savoie/Dropbox/orgs/")
    "Location of my .org mode files" )

  (when (and (file-accessible-directory-p mhs-org-mode-directory)
             (require 'mhs-org-mode))
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
  )



;; Yasnippet
;;-----------
;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-global-mode 1))


;; Replacement for edit-server?
;; https://github.com/alpha22jp/atomic-chrome
(use-package atomic-chrome
  :ensure t
  :config
  (atomic-chrome-start-server))

(provide 'emacs-sketchy-extras)
;;; EMACS-SKETCHY-EXTRAS.EL ends here
