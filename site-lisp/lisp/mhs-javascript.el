;;; mhs-javascript.el --- Modern TypeScript/JavaScript configuration
;;; Commentary:
;;; 100% claude make javascript/typescript work

;;; Code:

(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-mode . add-node-modules-path)
         (typescript-tsx-mode . add-node-modules-path)
         (js2-mode . add-node-modules-path)))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))


;; Smart parens
(when (featurep 'smartparens)
  (add-hook 'typescript-mode-hook #'smartparens-mode)
  (add-hook 'typescript-ts-mode-hook #'smartparens-mode))


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
