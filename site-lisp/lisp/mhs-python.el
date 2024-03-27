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
  :pin melpa
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq
   read-process-output-max (* 1024 1024)
   lsp-idle-delay 0.600
   lsp-pylsp-plugins-pydocstyle-enabled nil ;; while working on HGA (also now ruff handles this better?)
   lsp-pylsp-plugins-yapf-enabled nil
   lsp-pylsp-plugins-flake8-enabled nil
   lsp-pylsp-plugins-black-enabled t
   lsp-pylsp-plugins-autopep8-enabled nil
   lsp-pylsp-plugins-pycodestyle-enabled nil
   lsp-pylsp-plugins-pyflakes-enabled nil
   lsp-disabled-clients '((python-mode . pyls))
   )
  ;; This appears to allow me to set black and use preview, AKA new features
  (lsp-register-custom-settings
   '(
     ("pylsp.plugins.black.enabled" t)
     ("pylsp.plugins.black.preview" t)
     ))

  ;;

  ;; Need this for sideline info more grey.
  ;;   '(lsp-ui-sideline-symbol-info ((t (:extend t :background "#21242b" :foreground "#535960"))))
  ;; (set-face-attribute 'lsp-ui-sideline-symbol-info nil  :background "#21242b" :foreground "#535960")

  ;; Need this for lsp-breadcrumb faces looking too grey on the header line
  (set-face-attribute 'header-line nil :inherit 'mode-line :background "#71458f")

  ;; Set lsp-log-io to t for debugging and use lsp-workspace-show-log
  )



(use-package python-pytest
  :ensure t
  :after python
  :init
  ;; (define-key python-mode-map "\C-cT" 'python-pytest-dispatch)
  (define-key python-mode-map "\C-cy" 'python-pytest-dispatch)
  )
;; ## someday TODO [MHS, 03/20/2023]
;; (use-package! python-pytest
;;   :commands python-pytest-dispatch
;;   :init
;;   (map! :after python
;;         :localleader
;;         :map python-mode-map
;;         :prefix ("t" . "test")
;;         "a" #'python-pytest
;;         "f" #'python-pytest-file-dwim
;;         "F" #'python-pytest-file
;;         "t" #'python-pytest-function-dwim
;;         "T" #'python-pytest-function
;;         "r" #'python-pytest-repeat
;;         "p" #'python-pytest-dispatch
;;         "l" #'python-pytest-last-failed))


(use-package ivy :ensure t)
(use-package lsp-ivy
  :ensure t
  :pin melpa
  :after lsp)


(use-package lsp-treemacs
  :ensure t
  :after lsp)

(use-package lsp-ui
  :pin melpa
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq
   ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-update-mode 'point
   ;; ----------------------------------------
   lsp-ui-peek-enable t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-peek-height 25
   ;; ----------------------------------------
   lsp-ui-doc-enable t
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse nil
   )
  ;; Bad wrapping with sideline when you don't set this.
  (set-face-attribute 'markdown-code-face nil :inherit 'default)
  )


(use-package dap-mode
  :ensure t
  :pin melpa
  :hook
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :commands dap-debug
  :config
  (dap-auto-configure-mode 1)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (pyvenv-mode 1))

;; don't use flymake (elpy default), use flycheck
;; https://github.com/jorgenschaefer/elpy/issues/137#issuecomment-55403160
(use-package flycheck
  :ensure t)


(use-package ein
  :ensure t
  :init
  (setq ein:worksheet-enable-undo t)
  (setq ein:output-area-inlined-images t)
  (setq ein:polymode t)
  :config
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup))


(provide 'mhs-python)
;;; mhs-python.el ends here
