;;; mhs-javascript.el --- Modern TypeScript/JavaScript configuration
;;; Commentary:
;;; 100% claude make javascript/typescript work

;;; Code:

(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-mode . add-node-modules-path)
         (typescript-tsx-mode . add-node-modules-path)
         (js-ts-mode . add-node-modules-path)))

(use-package typescript-ts-mode
  :config
  (setq typescript-ts-mode-indent-offset 2
        tsx-ts-mode-indent-offset 2
        js-indent-level 2))


;; Smart parens
(with-eval-after-load 'smartparens
  (add-hook 'typescript-ts-mode-hook #'smartparens-mode)
  (add-hook 'tsx-ts-mode-hook #'smartparens-mode)
  (add-hook 'js-ts-mode-hook #'smartparens-mode))



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
