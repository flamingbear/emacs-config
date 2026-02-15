;;; mhs-lsp.el --- Shared LSP Mode Configuration

;; Need to install lsp in project you want to use this with.
;; pip install "python-lsp-server[all]" provides pylsp- options
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")

  :hook ((typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
  (setq read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.600
        lsp-completion-provider :none
	lsp-log-io nil)

  ;; Python-specific settings
  (setq lsp-disabled-clients '(pyls pylsp)
        lsp-pyright-multi-root nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t
        lsp-pyright-use-library-code-for-types t)

  (setq lsp-ruff-lsp-server-command '("ruff" "server"))

  ;; TypeScript-specific settings
  (setq lsp-typescript-format-enable t)


  ;; Corfu integration
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)

  ;; Cape integration for TypeScript
  (when (featurep 'cape)
    (add-hook 'lsp-mode-hook
              (lambda ()
                (setq-local completion-at-point-functions
                           (list (cape-capf-super
                                  #'lsp-completion-at-point
                                  #'cape-dabbrev
                                  #'cape-file))))))
  )

;; [MHS, 02/02/2026] We're just about to abandon eslint on this project, Let's simplify and use
;; it from commandline only for now...

;; (use-package lsp-eslint
;;   :after lsp-mode
;;   :config
;;   ;; this is not working for anything
;;   ;; (setq lsp-eslint-auto-fix-on-save t)
;;   ;; (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
;;   )


(use-package consult-lsp :ensure t
  :after lsp-mode)

(use-package lsp-treemacs :ensure t ;; :pin melpa
  :after lsp)

(use-package treemacs  :ensure t  ;;:pin melpa
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


(provide 'mhs-lsp)
;;; mhs-lsp.el ends here
