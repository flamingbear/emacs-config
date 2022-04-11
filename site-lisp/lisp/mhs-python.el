;; Below: Stolen from KWB totally^H^H^H^H^H mostly (well, originally anyway).

;; We're gonna need us a Python mode
(use-package python
  :config
  ;; Python is a dev mode
  (add-hook 'python-mode-hook 'run-dev-hook))

;; Need to install lsp in project you want to use this with.
;; pip install "python-lsp-server[all]" provides pylsp- options
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq
   read-process-output-max (* 1024 1024)
   lsp-idle-delay 0.500
   lsp-pylsp-plugins-pydocstyle-enabled t
   lsp-pylsp-plugins-yapf-enabled t
   lsp-pylsp-plugins-flake8-enabled t
   lsp-pylsp-plugins-pycodestyle-enabled nil
   lsp-pylsp-plugins-pyflakes-enabled nil
   lsp-disabled-clients '((python-mode . pyls))
   )
  ;; This appears to allow me to set black but I can't see the changes that are
  ;; going to be made.
  ;; (lsp-register-custom-settings
  ;;  '(
  ;;    ("pylsp.plugins.black.enabled" t)
  ;;    ("pylsp.plugins.black.preview" t)
  ;;    )
  ;;  )
  ;; Need this for lsp-breadcrumb faces looking too grey on the header line
  (custom-set-faces
   '(header-line ((t (:inherit mode-line :background "#71458f")))))

  )

  (use-package lsp-ivy
    :ensure t
    :after lsp)

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred


(use-package lsp-treemacs
  :ensure t
  :after lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-show-code-actions 'nil
   lsp-ui-sideline-update-mode 'point
   ;; ----------------------------------------
   lsp-ui-peek-enable t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-peek-height 25
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
  :hook
  (dap-stopped-hook . (lambda (arg) (call-interactively #'dap-hydra)))
  :commands dap-debug
  :config
  (dap-auto-configure-mode 1)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  )

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
