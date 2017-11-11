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

(use-package add-node-modules-path :ensure t
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook #'add-node-modules-path))
  (eval-after-load 'projectile-mode
    (add-hook 'projectile-after-switch-project-hook #'add-node-modules-path))
  )


(use-package js2-mode
  :ensure t
  :mode ("\\.jsx?\\'" . js2-jsx-mode)
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js-indent-level 2))

(use-package tern :ensure t
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (add-hook 'nodejs-repl-mode-hook (lambda () (tern-mode t))))

(use-package company-tern  :ensure t)

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
  :ensure t)

;; Declaring bankrupcy on my javascript mode until I start working in JS again.
(provide 'mhs-javascript)

;;; MHS-JAVASCRIPT.EL ends here
