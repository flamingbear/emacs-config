;; Below: Stolen from KWB totally^H^H^H^H^H mostly (well, originally anyway).

;; We're gonna need us a Python mode
(use-package python
  :config
  ;; Python is a dev mode
  (add-hook 'python-mode-hook 'run-dev-hook)
  )



(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred


(use-package python-pytest
  :ensure t
  :after python
  :init
  ;; (define-key python-mode-map "\C-cT" 'python-pytest-dispatch)
  (define-key python-mode-map "\C-cy" 'python-pytest-dispatch)
  )


(use-package dap-mode
  :ensure t
  ;; :pin melpa
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
