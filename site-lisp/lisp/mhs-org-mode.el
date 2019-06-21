;;; mhs-org-mode.el

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 14 Jan 2011
;; Version: 1.0
;; Keywords:

(use-package org
  :pin org
  :ensure org-plus-contrib

  :config
  ;; always open .org files in org-mode
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)

  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-agenda-files "~/Dropbox/orgs/org-agenda-files")

  ;; I don't want to see days in my time tracking
  (setq org-duration-format 'h:mm)

  ;; Optionally add notes or timestamps when you complete a task.
  (setq org-log-done (quote time))
  (setq org-todo-keywords '((sequence "TODO" "WAITING" "BLOCKED" "|" "DONE" "DELEGATED" "CANCELED")))

  (require 'org-capture)
  (define-key global-map "\C-cr" 'org-capture)
  (setq org-directory "~/Dropbox/orgs")
  (setq org-default-notes-file (concat org-directory "/notes.org"))


  (defvar mhs-org-mode-directory (expand-file-name "~savoie/Dropbox/orgs/")
    "Location of my .org mode files" )

  ;; Fancy set up for querying old archived agenda files.
  (when (and (file-accessible-directory-p mhs-org-mode-directory))
    (setq
     org-agenda-custom-commands
     '(("Q" . "Custom queries") ;; gives label to "Q"
       ("Qa" "Archive search" search "" ((org-agenda-files (file-expand-wildcards "~/Dropbox/orgs/*.org_archive"))))
       ("Qb" "Projects and Archive" search ""
        ((org-agenda-text-search-extra-files (file-expand-wildcards "~/Dropbox/orgs/*.org_archive"))))
       ;; searches both projects and archive directories
       ("QA" "Archive tags search" org-tags-view ""
        ((org-agenda-files (file-expand-wildcards "~/Dropbox/orgs/*.org_archive"))))))
    "mhs-org mode loaded")

  (setq org-fontify-done-headline nil)

  ;; Keep a clock across working sessions.
  (setq org-clock-persist (quote history))
  (setq org-clock-persist-file "~/Dropbox/orgs/org-clock-save.el")
  (org-clock-persistence-insinuate)
  ;;https://emacs.stackexchange.com/questions/38483/reminds-to-clock-out-or-just-clock-out-when-there-has-a-clock-running
  (defun my/org-clock-query-out ()
  "Ask the user before clocking out.
This is a useful function for adding to `kill-emacs-query-functions'."
  (if (and
       (featurep 'org-clock)
       (funcall 'org-clocking-p)
       (y-or-n-p "You are currently clocking time, clock out? "))
      (org-clock-out)
    t)) ;; only fails on keyboard quit or error
  (add-hook 'kill-emacs-query-functions 'my/org-clock-query-out)

  (defun mhs-update-today ()
    (interactive)
    (with-current-buffer (find-file-other-window "~/Dropbox/orgs/sprint_track.org")
      (save-excursion
	(text-mode)
	(beginning-of-buffer)
	(search-forward "TODAY")
	(beginning-of-line)
	(next-line)
	(while (search-forward-regexp "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (line-end-position) t)
	  (replace-match (format-time-string "%Y-%m-%d")))
	(org-mode)
	(org-clock-report)
	)
      (save-buffer)
      (org-show-all)
      ))

(provide 'mhs-org-mode)
;;; mhs-org-mode.el ends here
