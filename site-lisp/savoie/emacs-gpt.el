(use-package dall-e-shell
  :ensure t
  :config
  (setq dall-e-shell-openai-key
	(lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))


(use-package pcsv
  :ensure t)

;; check out chatgpt integration.
(use-package chatgpt-shell
  :ensure t
  :config
  (setq chatgpt-shell-model-version "gpt-3.5-turbo-0125")
  (setq chatgpt-shell-system-prompt 1)

  (setq chatgpt-shell-openai-key
	(lambda ()
          (auth-source-pick-first-password :host "api.openai.com")))

  ;; create a smart elisp tutor
  ;; https://www.reddit.com/r/emacs/comments/1aj5did/using_llms_to_fill_the_semantic_search_gap/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (add-to-list 'chatgpt-shell-system-prompts
	       '("Python Programming" . "The user is a python programmer. You treat their time as
precious. You do not repeat obvious things, including their
query.  You never apologize for confusions because it would waste
their time.  You use markdown liberally to structure responses.
Always show code snippets in markdown blocks with language
labels.  Don't explain code snippets unless asked.  Whenever you
output updated code for the user, only show diffs, instead of
entire snippets."))

  (add-to-list 'chatgpt-shell-system-prompts
	       '("Emacs Guru" . "You are an expert in all things Emacs. Help the user write
ideomatic code when asked.  Also suggest built-in functions when
possible.  End your response by approximating the coherence of
your reply from 0 to 100%.")))


;; Set up copilot
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  ;; https://github.com/copilot-emacs/copilot.el#2-configure-completion
  :config
  ;; Don't enable by default yet.
  ;; (add-hook 'prog-mode-hook #'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "C-p") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "C-h") 'copilot-help)
  )

(provide 'emacs-gpt)
