;;; mhs-magit.el ---                                 -*- lexical-binding: t; -*-



(setq magit-last-seen-setup-instructions "1.4.0")


;;; Code:
;; Originally idea for Github PR Stolen from here:
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
     (cdr (magit-get-remote-branch)))))



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


(provide 'mhs-magit)
;;; mhs-magit.el ends here
