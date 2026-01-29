;;; mhs-lsp.el --- Shared LSP Mode Configuration

;; Need to install lsp in project you want to use this with.
;; pip install "python-lsp-server[all]" provides pylsp- options
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")

  :hook ((python-mode . lsp)
         (typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))

  :config
  ;; General LSP settings
  (setq read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.600
        lsp-completion-provider :none)

  ;; Python-specific settings
  (setq lsp-disabled-clients '(pyls pylsp)
        lsp-pyright-multi-root nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t
        lsp-pyright-use-library-code-for-types t)

  ;; TypeScript-specific settings
  (setq lsp-typescript-preferences-import-module-specifier "relative"
        lsp-typescript-suggest-auto-imports t
        lsp-typescript-format-enable t)

  ;; Ruff setup
  (with-eval-after-load 'lsp-mode
    (setq lsp-ruff-lsp-server-command '("ruff" "server")))

  ;; Corfu integration
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)

  ;; Cape integration for TypeScript
  (when (featurep 'cape)
    (add-hook 'lsp-mode-hook
              (lambda ()
                (setq-local completion-at-point-functions
                           (list (cape-capf-super
                                  #'lsp-completion-at-point
                                  #'cape-dabbrev
                                  #'cape-file))))))
  )

(provide 'mhs-lsp)
;;; mhs-lsp.el ends here
