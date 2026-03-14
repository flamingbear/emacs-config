;; Functions to get api keys
(defun mhs/get-api-key (HOST)
  "Fetches the API key for the specified HOST."
  (auth-source-pick-first-password :host HOST))

(defun mhs/get-claude-anthropic-key ()
  "Fetches the API key for Claude Anthropic."
  (mhs/get-api-key "api.anthropic.com"))

(defun mhs/get-openai-key ()
  "Fetches the API key for OpenAI. gpt"
  (mhs/get-api-key "api.openai.com"))


(use-package gptel
  :ensure t
  ;;  :pin melpa-stable
  :config

  ;;Reddit comment on how make claude less verbose:
  ;; https://www.reddit.com/r/ClaudeAI/comments/1gn8bpf/comment/lw8l3q8/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ;; if you want to have prompts and that, you can just highlight the prompt and add it to the context
  (setq
   gptel-default-mode 'markdown-mode ;; 'org-mode
   gptel-model 'claude-sonnet-4-6
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
  (gptel-make-gh-copilot "Copilot")
  )


;; use with a .claude/docker/Dockerfile to build an image claude-agent-acp
;;-----------------------
;; FROM node:22-slim
;; RUN npm install -g @zed-industries/claude-agent-acp@0.21.0
;; ENTRYPOINT ["claude-agent-acp"]

;; Also include an encrypted copy of your oauth token genrated with `claude
;; setup-token` in .claude/docker/claude_container_oauth_token.gpg

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-session-strategy 'prompt)
  (setq agent-shell-command-prefix
        (lambda (buffer)
          "Run claude-agent-acp in an isolated Docker container for the current project."
          (let* ((project-dir (with-current-buffer buffer
                                (or (vc-root-dir)
                                    default-directory)))
                 (claude-dir (expand-file-name "~/.claude")))
            (list (expand-file-name "~/.claude/docker/run.sh")
                  "-v" (concat project-dir ":/workspace")
                  "-v" (concat claude-dir ":/root/.claude")
                  "-w" "/workspace"
                  "claude-agent-acp"))))

  (setq agent-shell-path-resolver-function
        (lambda (path)
          "Map paths between /workspace/ in container and local project dir."
          (let ((cwd (or (vc-root-dir)
                         default-directory)))
            (cond
             ;; Local path -> container path
             ((string-prefix-p cwd path)
              (concat "/workspace/" (string-remove-prefix cwd path)))
             ;; Container path -> local path
             ((string-prefix-p "/workspace/" path)
              (concat cwd (string-remove-prefix "/workspace/" path)))
             (t path))))))

(provide 'emacs-gpt)
