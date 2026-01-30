;; Below: Stolen from KWB totally^H^H^H^H^H mostly (well, originally anyway).


;; Switch to tree-sitter mode [claude says so] 2026-01-29
(when (treesit-language-available-p 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(when (featurep 'smartparens)
  (add-hook 'python-base-mode-hook #'smartparens-mode))

;; We're gonna need us a Python mode
(use-package python
  :config
  :hook (python-base-mode . run-dev-hook))


(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook (python-base-mode . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))  ; or lsp-deferred (was lsp)


(use-package python-pytest
  :ensure t
  :after python
  :init
  (define-key python-mode-map "\C-cy" 'python-pytest-dispatch))


(use-package dap-mode
  :ensure t
  ;; :pin melpa
  :defer t
  :hook
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :commands dap-debug
  :config
  ;; Disable UI features that cause the hang
  (setq dap-auto-configure-features '(sessions repl tooltip breakpoints))

  ;; Completely disable UI elements that might be problematic
  ;; (setq dap-ui-controls-enable nil)

  ;; (dap-auto-configure-mode 1)

  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  ;; Debug this stuff
  ;; (setq dap-print-io t)

  ;; Configure a simplified template
  (dap-register-debug-template
   "Python :: Debug Run file (buffer)"
   (list :type "python"
         :args ""
         :cwd "${workspaceFolder}"
         :module nil
         :program nil
         :request "launch"
         :name "Python :: Debug Run file (buffer)"))
  )

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))


(provide 'mhs-python)
;;; mhs-python.el ends here
