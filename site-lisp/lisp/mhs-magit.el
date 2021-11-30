;;; mhs-magit.el ---                                 -*- lexical-binding: t; -*-

(use-package git-timemachine :ensure t :defer 3)
(use-package git-messenger :ensure t :defer 3
  :config
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message))

(use-package git-gutter
  :ensure
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode 't))

(use-package git-link :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil)
  (setq magit-diff-refine-hunk 'all)
  ;; work around for bug. https://github.com/magit/ghub/issues/81
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  ;; http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
  (defun endless/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*")
        (magit-remotes
         (magit-get-all "remote" "origin" "fetch")))
    (unless (or (not magit-remotes)
                (member fetch-address magit-remotes))
      (when (string-match
             "github" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

  (add-hook 'magit-mode-hook #'endless/add-PR-fetch)

  ;; Code:
  ;; Originally idea for Github PR Stolen (well) from here:
  ;; http://endlessparentheses.com/easily-create-github-prs-from-magit.html?source=rss

  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (let ((repo (magit-get "remote" (magit-get-remote) "url")))
      (if (string-match "github\\.com" repo)
          (visit-gh-pull-request repo)
        (visit-bb-pull-request repo))))



  (defun visit-gh-pull-request (repo)
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              repo)
             (magit-get-current-branch))))



  ;; Bitbucket pull requests are kinda funky, it seems to try to just do the
  ;; right thing, so there's no branches to include.
  ;; https://bitbucket.org/nsidc/measures-byu-vm/pull-request/new
  (defun visit-bb-pull-request (repo)
    (browse-url
     (format "https://bitbucket.org/%s/pull-request/new"
             (replace-regexp-in-string
              "\\`.+bitbucket\\.org:\\(.+\\)\\.git\\'" "\\1"
              repo))))

  ;; browse to you CI machine if it is created normally.
  (defun mhs/visit-ci-machine ()
    "browse to this project's CI jenkins instance."
    (interactive)
    (browse-url (format "http://ci.%s.apps.int.nsidc.org:8080/"
                        (replace-regexp-in-string
                         "\\`.+bitbucket\\.org:nsidc\/\\(.+\\)\\.git\\'" "\\1"
                         (magit-get "remote" (magit-get-remote) "url")))))


  ;; visit PR for github or bitbucket repositories with "V"
  (eval-after-load 'magit
    '(define-key magit-mode-map "v"
       #'endless/visit-pull-request-url))

  (eval-after-load 'magit
    '(define-key mhs-map "c"
       #'mhs/visit-ci-machine))

  (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package forge
  :after magit
  :ensure t)

(use-package github-review
  :ensure t)

(provide 'mhs-magit)
;;; mhs-magit.el ends here
