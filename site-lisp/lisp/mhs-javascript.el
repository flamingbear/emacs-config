;;; mhs-javascript.el --- Modern TypeScript/JavaScript configuration
;;; Commentary:

;;; 100% claude make javascript/typescript work

;;; Code:

;; Add node_modules to path
(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-mode . add-node-modules-path)
         (typescript-tsx-mode . add-node-modules-path)
         (js2-mode . add-node-modules-path)))

;; Tree-sitter based TypeScript modes (Emacs 29+)
(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

;; LSP for TypeScript
(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js2-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-typescript-preferences-import-module-specifier "relative"
        lsp-typescript-suggest-auto-imports t
        lsp-typescript-format-enable t)
  ;; Add LSP completion to Cape for Corfu
  (when (featurep 'cape)
    (add-hook 'lsp-mode-hook
              (lambda ()
                (setq-local completion-at-point-functions
                           (list (cape-capf-super
                                  #'lsp-completion-at-point
                                  #'cape-dabbrev
                                  #'cape-file)))))))

;; Keep js2-mode for fallback/legacy JS
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2
        js-indent-level 2
        js-switch-indent-offset 2
        indent-tabs-mode nil))

;; JSX/TSX support
(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

;; Web-mode as fallback for complex JSX
(use-package web-mode
  :ensure t
  :mode (("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-content-types-alist '(("jsx" . "\\.jsx?\\'"))))

;; Smart parens
(when (featurep 'smartparens)
  (add-hook 'typescript-mode-hook #'smartparens-mode)
  (add-hook 'typescript-ts-mode-hook #'smartparens-mode))

;; REMOVED: company-mode configuration (conflicts with Corfu)

;; REPL for Node.js
(use-package nodejs-repl
  :ensure t
  :bind (:map typescript-ts-mode-map
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-z" . nodejs-repl-switch-to-repl))
  :config
  (setq nodejs-repl-arguments '("--experimental-repl-await")))

(provide 'mhs-javascript)
;;; mhs-javascript.el ends here
