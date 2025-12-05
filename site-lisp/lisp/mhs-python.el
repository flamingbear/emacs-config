;; Below: Stolen from KWB totally^H^H^H^H^H mostly (well, originally anyway).

;; We're gonna need us a Python mode
(use-package python
  :config
  ;; Python is a dev mode
  (add-hook 'python-mode-hook 'run-dev-hook)
  )

;; Need to install lsp in project you want to use this with.
;; pip install "python-lsp-server[all]" provides pylsp- options
(use-package lsp-mode
  :ensure t
  ;;  :pin melpa
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq
   read-process-output-max (* 1024 1024)
   lsp-idle-delay 0.600
   lsp-disabled-clients '(pyls pylsp)
   ;; lsp-log-io t   ;; set for debugging
   )

  ;; Pyright specific settings
  (setq lsp-pyright-multi-root nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t
        lsp-pyright-use-library-code-for-types t)

  (with-eval-after-load 'lsp-mode
    (setq lsp-ruff-lsp-server-command '("ruff" "server")))

  ;; (with-eval-after-load 'lsp-mode
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "ruff" "server")
  ;;                   :activation-fn (lsp-activate-on "python")
  ;;                   :server-id 'ruff
  ;;                   :multi-root nil)))



  ;; Flinging reddit snippets at the wall
  ;; https://www.reddit.com/r/emacs/comments/ql8cyp/corfu_orderless_and_lsp/
  ;; error was: "LSP :: Unable to autoconfigure company-mode."
  (setq lsp-completion-provider :none)
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
		completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)


)


(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; (use-package ruff-format
;;   :ensure t
;;   :after python
;;   :config
;;   (add-hook 'python-mode-hook 'ruff-format-on-save-mode)
;;   )


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


(use-package ivy
  :ensure t
  )
(use-package lsp-ivy
  :ensure t
  ;; :pin melpa
  :after lsp)


(use-package lsp-treemacs
  :ensure t
  ;; :pin melpa
  :after lsp)

(use-package treemacs
  :ensure t
  ;;:pin melpa
  )

(use-package lsp-ui
  ;; :pin melpa
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

;; try PET?
;; Way to f'ing slow
;; (use-package pet
;;   :ensure t
;;   :config
;;   (add-hook 'python-base-mode-hook 'pet-mode -10))


;; don't use flymake (elpy default), use flycheck
;; https://github.com/jorgenschaefer/elpy/issues/137#issuecomment-55403160
(use-package flycheck
  :ensure t)


;; (use-package ein
;;   :ensure t
;;   :init
;;   (setq ein:worksheet-enable-undo t)
;;   (setq ein:output-area-inlined-images t)
;;   (setq ein:polymode t)
;;   :config
;;   (add-hook 'ein:connect-mode-hook 'ein:jedi-setup))


(provide 'mhs-python)
;;; mhs-python.el ends here
