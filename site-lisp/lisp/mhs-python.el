;; Below: Stolen from KWB totally^H^H^H^H^H mostly (well, originally anyway).


;; Switch to tree-sitter mode [claude says so] 2026-01-29
(when (treesit-language-available-p 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(with-eval-after-load 'smartparens
  (add-hook 'python-base-mode-hook #'smartparens-mode))

;; We're gonna need us a Python mode
(use-package python)


(use-package lsp-pyright
  :ensure t
  :defer t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook (python-base-mode . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))  ; or lsp-deferred (was lsp)


(use-package python-pytest
  :ensure t
  :after python
  :bind (:map python-base-mode-map
              ("C-c y" . python-pytest-dispatch)))


(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))


(provide 'mhs-python)
;;; mhs-python.el ends here
