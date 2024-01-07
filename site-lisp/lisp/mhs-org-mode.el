;;; mhs-org-mode.el

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 14 Jan 2011
;; Version: 1.0
;; Keywords:

;; some combination of the two below gave me what I needed to update for new org locations.
;; https://www.reddit.com/r/emacs/comments/r11nqd/how_to_install_orgmode_now_that_org_emacs_lisp/
;; https://github.com/jwiegley/use-package/issues/319#issuecomment-845214233
;; (assq-delete-all 'org package--builtins)
;; (assq-delete-all 'org package--builtin-versions)


(use-package org
  :pin gnu
  :ensure t
  :config
  ;; always open .org files in org-mode
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)

  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-agenda-files "~/Dropbox/orgs/org-agenda-files")

  ;; indent sanely?
  (setq org-startup-folded 't)
  (setq org-adapt-indentation 't)
  (setq org-startup-indented 't)

  ;; hide org markdown around ~code~ +strikethrough+ /italics/
  (setq org-hide-emphasis-markers 't)

  ;; I don't want to see days in my time tracking
  (setq org-duration-format 'h:mm)

  ;; Where can you refile things
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  ;; These are questionable...
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Optionally add notes or timestamps when you complete a task.
  (setq org-log-done (quote time))
  (setq org-todo-keywords '((sequence "TODO" "WAITING" "BLOCKED" "|" "DONE" "DELEGATED" "CANCELED")))

  (require 'org-capture)
  (define-key global-map "\C-cr" 'org-capture)
  (setq org-directory "~/Dropbox/orgs")
  (setq org-default-notes-file (concat org-directory "/notes.org"))


  (defvar mhs-org-mode-directory (expand-file-name "~savoie/Dropbox/orgs/")
    "Location of my .org mode files" )

  ;; Extra template expansions
  (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

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

  (setq org-fontify-done-headline 'nil)

  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit '(fixed-pitch shadow))

  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(org-block ((t (:inherit fixed-pitch))))
  ;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-document-info ((t (:foreground "dark orange"))))
  ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  ;;  '(org-link ((t (:foreground "#8AB4F7" :underline t))))
  ;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


  ;; Keep a clock across working sessions.
  (setq org-clock-persist 'history)
  (setq org-clock-persist-file "~/Dropbox/orgs/org-clock-save.el")
  (org-clock-persistence-insinuate)

  ;;https://emacs.stackexchange.com/questions/38483/reminds-to-clock-out-or-just-clock-out-when-there-has-a-clock-running
  ;; (defun my/org-clock-query-out ()
  ;;   "Ask the user before clocking out. This is a useful function for adding to `kill-emacs-query-functions'."
  ;;   (if (and (featurep 'org-clock)
  ;; 	     (funcall 'org-clocking-p))
  ;; 	(org-clock-out)
  ;;    t)) ;; only fails on keyboard quit or error
  ;; (add-hook 'kill-emacs-query-functions 'my/org-clock-query-out)

  ;; Updates sprint track files with today's hours.
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
  )
(use-package org-contrib :pin nongnu :ensure t)

(use-package org-jira
  :ensure t
  :after org
  :config
  (setq
   jiralib-url "https://bugs.earthdata.nasa.gov"
   org-jira-working-dir "/Users/savoie/Dropbox/orgs/.org-jira" ;; (expand-file-name ".org-jira" user-emacs-directory))
   )
  )
(provide 'mhs-org-mode)
;;; mhs-org-mode.el ends here
