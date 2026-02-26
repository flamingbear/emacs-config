;;; mhs-magit.el ---                                 -*- lexical-binding: t; -*-

(use-package git-timemachine :ensure t :defer 3)
(use-package git-messenger :ensure t :defer 3
  :config
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message))

;; (use-package git-gutter
;;   :ensure
;;   :diminish git-gutter-mode
;;   :init
;;   (global-git-gutter-mode 't))
;; git gutter alternative?
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package git-link
  :ensure t
  :config
  (setq git-link-open-in-browser t)
  (setq git-link-use-single-line-number t)
  )

(use-package magit
  :ensure t
  :defer t
  :after exec-path-from-shell
  :config
  (setq magit-git-executable (executable-find "git")
        magit-diff-visit-prefer-worktree t)

  ;; (setq magit-diff-refine-hunk 'all)

  ;; https://emacs.stackexchange.com/questions/28537/a-way-to-insert-a-predefined-text-into-magits-commit-message-window
  ;; https://emacs.stackexchange.com/a/44685/613
  (defun mhs/parse-current-branch ()
    (let ((ISSUEKEY "[[:alpha:]]+-[[:digit:]]+")
          (branch (magit-get-current-branch)))
      (when (string-match (concat "\\(?:^\\|mhs/\\)\\(" ISSUEKEY "\\)") branch)
	(match-string 1 branch))))

  ;; return the ticket parsed from the branch or my current ticket number
  (defun mhs/current-ticket ()
    (or (mhs/parse-current-branch)
        mhs-jira--current-ticket-number))

  (defun my-git-commit-setup ()
    (let ((ticket (mhs/current-ticket)))
      (when (and ticket (not (string-empty-p ticket)))
        (insert (concat ticket ": " )))))

  (add-hook 'git-commit-setup-hook 'my-git-commit-setup)
  ;; (setq magit-completing-read-function 'completing-read)
  )

(use-package forge
  :after magit
  ;; :pin melpa-stable
  :ensure t
  ;;  :config
  ;; If BB was supported this should work.
  ;; (add-to-list
  ;;  'forge-alist
  ;;  '("git.earthdata.nasa.gov" "api.bitbucket.org/2.0" "git.earthdata.nasa.gov" forge-bitbucket-repository))
  )

(use-package sqlite3
  :ensure t)

(use-package github-review
  :ensure t)

(provide 'mhs-magit)
;;; mhs-magit.el ends here
