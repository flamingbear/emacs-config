;;; mhs-javascript.el --- Tweaks for Javascript coding in Emacs.
;;; Commentary: Mostly Stolen from https://github.com/w0hrk/kwb-emacs/blob/master/dev-modes/dev-javascript.el
;; -----------------
;; JavaScript
;; -----------------
;; Prerequisite Emacs packages:
;;   * add-node-modules-path
;;   * company
;;   * company-tern
;;   * exec-path-from-shell
;;   * flycheck
;;   * js2-mode
;;   * nodejs-repl
;;   * tern
;; Prerequisite eslint:
;;   ./node_modules/.bin/eslint --init
;;     answer questions about your linting prefs.
;; Prerequisite NPM packages:
;;   * ES5:
;;     * nvm
;;     * NodeJS
;;     * npm install eslint tern --save-dev
;;   * ES6:
;;     * ES5, and
;;     * npm install babel-cli babel-preset-es2015 --save-dev
;;   * React:
;;     * ES5 or ES6, and
;;     * npm install eslint-plugin-react --save-dev
;;     * npm install babel-preset-react --save-dev
;; Notes:
;;   When using babeljs for ES6, set a dir local variable so you
;;   can use the nodejs-repl via C-c C-z or M-x nodejs-repl.  The
;;   project's .dir-locals.el file should be at the top of the
;;   project and contain something like this:
;;     ((js2-mode (nodejs-repl-command
;;                 .
;;                 "/home/kwbeam/labs/js-lab/node_modules/.bin/babel-node")))
;;   Where (obviously) you need to change the absolute path.
;;   TODO:
;;     * set nodejs-repl-command by finding it in the project's directory tree!

;;; Code:

(defconst mhs-javascript-version "0.2.0"
  "Report bugs to: Matt Savoie <emacs@flamingbear.com>.")

;; 2018-11-03 - this might not be what we want, and could be causing the
;; problems with old lint in my cumulus repo dependencies. causing me to write
;; the mhs/use-eslint-from-project-root-node-modules hack.
(use-package add-node-modules-path :ensure t
  :config
  (eval-after-load 'js2-mode '(add-hook 'js2-mode-hook #'add-node-modules-path))
  (eval-after-load 'projectile-mode (add-hook 'projectile-after-switch-project-hook #'add-node-modules-path)))

(use-package js2-mode
  :ensure t
  :mode ("\\.jsx?\\'" . js2-jsx-mode)
  :mode ("\\.js\\'" . js2-mode)
  :config
  (add-hook 'js2-mode-hook (lambda ()  (setq indent-tabs-mode nil)))
  (setq js-switch-indent-offset 2)
  (setq js-indent-level 2))


;; Insert pretty javadocs.
(use-package js-doc
  :ensure t
  :config
  (setq js-doc-mail-address "savoie@nsidc.org"
	js-doc-author (format "Matt Savoie <%s>" js-doc-mail-address)
	js-doc-url ""
	js-doc-license "")

  (add-hook 'js2-mode-hook
            #'(lambda ()
		(define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
		(define-key js2-mode-map "@" 'js-doc-insert-tag))))



;; Can't use prettier-js with an existing package that uses different eslint
;; (use-package prettier-js :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook 'prettier-js-mode)
;;   (setq prettier-js-args '(
;; 			   "--trailing-comma" "all"
;; 			   "--bracket-spacing" "true"
;; 			   "--single-quote" "true"
;; 			   )) )

;; This might come in handy
(use-package eslint-fix :ensure t)

;; Let's try smart parens in javascript.
(when (featurep 'smartparens)
    (add-hook 'js2-mode-hook #'smartparens-mode))


;; Use Tide for javascript goodness. It supports linting, rename, find
;; references/go to definition, and look up documentation.
(defun setup-tide-mode ()
  "Setup tide mode for js"
  (interactive)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (eldoc-mode +1)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq company-tooltip-align-annotations t))
  ;; to debug tide-mode
  ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /Users/savoie/tmp/tss.log"))

(use-package tide
  :ensure t
  :after (company flycheck js2-mode)
  :hook ((js2-mode . tide-setup))
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(use-package nodejs-repl :ensure t
  :config
  ;; Setup key mappings for nodejs-repl.
  (add-hook 'js2-mode-hook '(lambda ()
			      (local-set-key "\C-x\C-e" 'nodejs-repl-send-last-sexp)
			      (local-set-key "\C-cr" 'nodejs-repl-send-region)
			      (local-set-key "\C-cb" 'nodejs-repl-send-buffer)
			      (local-set-key "\C-cl" 'nodejs-repl-load-file)
			      (local-set-key "\C-c\C-z" 'nodejs-repl-switch-to-repl))))


(use-package indium
  :ensure t
  :after js2-mode
  :hook
  (js2-mode . indium-interaction-mode)
  :config
  (setq indium-client-debug 't)
  )

(provide 'mhs-javascript)

;;; MHS-JAVASCRIPT.EL ends here
