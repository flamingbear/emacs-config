;; Below: Stolen from KWB totally^H^H^H^H^H mostly (well, originally anyway).

;; We're gonna need us a Python mode
(use-package python
  :config
  ;; Python is a dev mode
  (add-hook 'python-mode-hook 'run-dev-hook))

(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq
   read-process-output-max (* 1024 1024)
   lsp-idle-delay 0.500
   lsp-pyls-plugins-pydocstyle-enabled t
   lsp-pyls-plugins-yapf-enabled t
   lsp-pyls-plugins-flake8-enabled t
   lsp-pyls-plugins-pycodestyle-enabled nil
   lsp-pyls-plugins-pyflakes-enabled nil
   lsp-keymap-prefix "C-c C-l")
  )

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-update-mode 'point
   ;; ----------------------------------------
   lsp-ui-peek-enable t
   ;; lsp-ui-peek-list-width 60
   ;; lsp-ui-peek-peek-height 25
   ;; ----------------------------------------
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'bottom
   lsp-ui-doc-delay 0
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-enable t
   lsp-ui-doc-use-childframe t
   )
  ;; Bad wrapping with sideline when you don't set this.
  (custom-set-faces
   '(markdown-code-face ((t (:inherit default)))))
  )

(use-package dap-mode
  :ensure t
  :config
  (dap-auto-configure-mode 1)
  ;; (dap-ui-mode 1)
  ;; (dap-tooltip-mode 1)
  (setq dap-python-debugger 'debugpy)
  (require 'dap-python))


(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))


;; don't use flymake (elpy default), use flycheck
;; https://github.com/jorgenschaefer/elpy/issues/137#issuecomment-55403160
(use-package flycheck
  :ensure t
  )


(use-package ein
  :ensure t
  :init
  (setq ein:completion-backend 'ein:use-company-backend)
  :config
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  )


(provide 'mhs-python)
;;; mhs-python.el ends here
