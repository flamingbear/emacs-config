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
  (unless (string= (system-name) "gridz.local")
    (add-hook 'after-init-hook (lambda ()
				 (org-agenda-list)
				 (org-clock-goto))))

  ;; always open .org files in org-mode
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)

  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-agenda-files "~/Dropbox/orgs/org-agenda-files")

  ;; New bug?
  (setq tab-width 8)

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

  (require 'org-id)
  (setq org-id-link-to-org-use-id t)

  (require 'org-capture)
  (define-key global-map "\C-cr" 'org-capture)
  (setq org-directory "~/Dropbox/orgs")
  (setq org-default-notes-file (concat org-directory "/notes.org"))



  (defun mhs/org-capture-template-function ()
    (let (
	  (target-path `("~/Dropbox/orgs/data-services.org" ,(concat "PI " (mhs/current-pi)) "Catchup / MISC / overhead"))
	  (meeting-path `("~/Dropbox/orgs/data-services.org" ,(concat "PI " (mhs/current-pi)) "Meetings"))
	  (task-path `("~/Dropbox/orgs/data-services.org" ,(concat "PI " (mhs/current-pi)) "Tasks"))
	  (retro-target-path `("~/Dropbox/orgs/data-services.org" ,(concat "PI " (mhs/current-pi)) "Retro Notes"))
	  (tracking-timeoff `("~/Dropbox/orgs/time_off.org" ,(concat "Time Off Tracking " (format-time-string "%Y")) "Vacation"))
	  )
      `(("b" "Note for Boss" entry (file+headline "~/Dropbox/orgs/daac.org" "Erik 1-1")
	 "* TODO %? \n  %U\n  %i\n  %a")
	("r" "Retrospective idea" entry (file+olp ,@retro-target-path)
	 "*  %? \n")
	("o" "TODO Note" entry (file+headline "" "Tasks")
	 "* TODO %?\n  %u\n  %a")
	("n" "Do Not Forget Note" entry (file+headline "" "Tasks")
	 "* %?\n  %u")
	("m" "Meeting" entry (file+olp ,@meeting-path)
	 "* %^{Meeting Name|}\n %? " :clock-in t :clock-keep t)
	("t" "Task" entry (file+olp ,@task-path)
	 "* %^{Task|}\n %? " :clock-in t :clock-keep t)
	("v" "Vacation" entry (file+olp ,@tracking-timeoff)
	 "* %^{Vacation?}\n:LOGBOOK:\nCLOCK: [%<%Y-%m-%d %a> 08:00]--[%<%Y-%m-%d %a> 17:00] => 09:00\n:END:\n")
	)))

  (setq org-capture-templates
	(mhs/org-capture-template-function))

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
	(org-clock-report))
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

;; use git flavored markdown for exporting
(use-package ox-gfm
  :ensure t
  :after org
  :config
  (eval-after-load "org"
    '(require 'ox-gfm nil t))
  )


(provide 'mhs-org-mode)
;;; mhs-org-mode.el ends here
