;; Functions to get api keys
(defun mhs/get-api-key (HOST)
  "Fetches the API key for the specified HOST."
  (auth-source-pick-first-password :host HOST))

(defun mhs/get-claude-anthropic-key ()
  "Fetches the API key for Claude Anthropic."
  (mhs/get-api-key "api.anthropic.com"))

(defun mhs/get-openai-key ()
  "Fetches the API key for OpenAI."
  (mhs/get-api-key "api.openai.com"))

(use-package gptel
  :ensure t
  :config
  ;; expensive deep thinking claude-3-opus-20240229

  ;;Reddit comment on how make claude less verbose:
  ;; https://www.reddit.com/r/ClaudeAI/comments/1gn8bpf/comment/lw8l3q8/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ;; if you want to have prompts and that, you can just highlight the prompt and add it to the context
  (setq
   gptel-default-mode 'org-mode ;markdown-mode
   gptel-model 'claude-3-5-sonnet-20241022
   gptel-backend (gptel-make-anthropic "Claude"          ;Any name you want
		   :stream t                             ;Streaming responses
		   :key 'mhs/get-claude-anthropic-key))
  (add-to-list 'gptel-directives
               '(geophysics . "You are a geophysical programmer, \
deeply familiar with Python and the libraries xarray, netCDF, rasterio, and rio-xarray. \
Provide detailed, accurate, and highly efficient code solutions."))
  (add-to-list 'gptel-directives
               '(emacs . "You are a an emacs wizard, familiar with org-mode, elisp and emacs itself. \
Help the user write idiomatic code, suggesting built-in functions when possible."))
    (add-to-list 'gptel-directives
               '(tech-writer . "You are a scientific technical writer, edit items for clarity and understanding. Show you changes as diff output"))
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  )


(use-package dall-e-shell
  :ensure t
  :defer t
  :config
  (setq dall-e-shell-openai-key 'mhs/get-openai-key))


(use-package pcsv :ensure t)

;; check out chatgpt integration.
(use-package chatgpt-shell
  :ensure t
  :defer t
  :config
  (setq chatgpt-shell-model-version "gpt-3.5-turbo-0125"
        chatgpt-shell-system-prompt 1
        chatgpt-shell-openai-key 'mhs/get-openai-key)

  ;; create a smart elisp tutor
  ;; https://www.reddit.com/r/emacs/comments/1aj5did/using_llms_to_fill_the_semantic_search_gap/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (add-to-list 'chatgpt-shell-system-prompts
	       '("Python Programming" . "The user is a python programmer. You treat their time as \
precious. You do not repeat obvious things, including their \
query.  You never apologize for confusions because it would waste \
their time.  You use markdown liberally to structure responses. \
Always show code snippets in markdown blocks with language \
labels.  Don't explain code snippets unless asked.  Whenever you \
output updated code for the user, only show diffs, instead of \
entire snippets."))

  (add-to-list 'chatgpt-shell-system-prompts
	       '("Emacs Guru" . "You are an expert in all things Emacs. Help the user write \
ideomatic code when asked.  Also suggest built-in functions when \
possible.  End your response by approximating the coherence of \
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
  :defer t
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
