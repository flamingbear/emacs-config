;;; mhs-dap.el --- DAP (Debug Adapter Protocol) configuration  -*- lexical-binding: t; -*-

;; Unified dap-mode config for Python and Node.js debugging.
(use-package dap-mode
  :ensure t
  :defer t
  :commands dap-debug
  :config
  (setq dap-auto-configure-features '(sessions locals repl tooltip breakpoints))

  ;; Python debugger
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  (dap-register-debug-template
   "Python :: Debug Run file (buffer)"
   (list :type "python"
         :args ""
         :cwd "${workspaceFolder}"
         :module nil
         :program nil
         :request "launch"
         :name "Python :: Debug Run file (buffer)"))

  ;; Attach to a running Python process (e.g. a Jupyter kernel) that has
  ;; started a debugpy server with:
  ;;   import debugpy; debugpy.listen(5678)   # ("0.0.0.0", 5678) in a container
  ;; Usage:
  ;;   1. Set breakpoints in your .py file (C-c C-d b b)
  ;;   2. M-x dap-debug -> "Python :: Attach (debugpy 5678)"
  ;;   3. Run the kernel cell that calls into your .py file; it will pause here.
  ;; No :pathMappings here: a local kernel reports absolute paths that already
  ;; match the buffer, so mapping only breaks breakpoint binding.  If the kernel
  ;; runs in Docker, use the container template below instead.
  (dap-register-debug-template
   "Python :: Attach (debugpy 5678)"
   (list :type "python"
         :request "attach"
         :connect (list :host "127.0.0.1" :port 5678)
         :justMyCode :json-false
         :name "Python :: Attach (debugpy 5678)"))

  ;; Same, but for a kernel running inside Docker.  Adjust :remoteRoot to the
  ;; container working directory where it sees your .py file (e.g. /workspace).
  (dap-register-debug-template
   "Python :: Attach (debugpy 5678, Docker)"
   (list :type "python"
         :request "attach"
         :connect (list :host "127.0.0.1" :port 5678)
         :pathMappings (vector (list :localRoot "${workspaceFolder}"
                                     :remoteRoot "/workspace"))
         :justMyCode :json-false
         :name "Python :: Attach (debugpy 5678, Docker)"))

  ;; Node.js debugger
  (require 'dap-node)
  (dap-node-setup) ;; downloads vscode-node-debug2 adapter on first run

  ;; Attach to Harmony (ts-node-dev --inspect=127.0.0.1:9200)
  ;; Start with: npm run start-dev-fast
  ;; Then M-x dap-debug and pick "Attach to Harmony"
  (dap-register-debug-template
   "Attach to Harmony Service"
   (list :type "node"
         :request "attach"
         :port 9200
         :address "127.0.0.1"
         :restart t
         :sourceMaps t
         :smartStep t
         :skipFiles ["<node_internals>/**" "**/node_modules/**"]
         :resolveSourceMapLocations ["!**/node_modules/**" "**"]
         :name "Attach to Harmony"))

  (dap-register-debug-template
   "Attach to Work Updater Service"
   (list :type "node"
         :request "attach"
         :port 9303
         :address "127.0.0.1"
         :restart t
         :sourceMaps t
         :smartStep t
         :skipFiles ["<node_internals>/**" "**/node_modules/**"]
         :resolveSourceMapLocations ["!**/node_modules/**" "**"]
         :name "Attach to Work Updater"))


  ;; Attach to any running Node process by PID
  (dap-register-debug-template
   "Node :: Attach by PID"
   (list :type "node"
         :request "attach"
         :processId "${command:PickProcess}"
         :sourceMaps t
         :smartStep t
         :skipFiles ["<node_internals>/**" "**/node_modules/**"]
         :name "Node :: Attach by PID"))

  ;; Attach to a test process started with NODE_OPTIONS='--inspect-brk=9229'
  ;; Usage:
  ;;   1. Set breakpoints in your .ts file
  ;;   2. go to the service directory and run mocha directly.
  ;;    > TS_NODE_TRANSPILE_ONLY=true node --inspect-brk=9229 ./node_modules/.bin/mocha test/util/log.ts
  ;;   3. M-x dap-debug → "Node :: Attach to Test (9229)"
  (dap-register-debug-template
   "Node :: Attach to Test (9229)"
   (list :type "node"
         :request "attach"
         :port 9229
         :address "127.0.0.1"
         :restart nil
         :sourceMaps t
         :smartStep t
         :skipFiles ["<node_internals>/**" "**/node_modules/**"]
         :resolveSourceMapLocations ["!**/node_modules/**" "**"]
         :name "Node :: Attach to Test (9229)"))

  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (company-mode -1)
              (setq-local completion-at-point-functions
                          (list (cape-company-to-capf #'dap-ui-repl-company)))))

  (global-set-key (kbd "<f10>")    'dap-hydra) ; F10   ;; ORYX Debug Layer - Y
  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (add-hook 'post-command-hook
                        (lambda ()
                          (when (fboundp 'dap-hydra/nil)
                            (dap-hydra/nil)))
                        nil t))))

;; DAP prefix keymap at C-c C-d
;; Mirrors the dap-hydra heads so all commands are reachable without the hydra.
;; These will be added to a debug layer in ORYX on my moonlander.
(defvar mhs-dap-map     (make-sparse-keymap) "DAP top-level commands.")
(defvar mhs-dap-s-map   (make-sparse-keymap) "DAP session/stack commands (s prefix).")
(defvar mhs-dap-b-map   (make-sparse-keymap) "DAP breakpoint commands (b prefix).")
(defvar mhs-dap-d-map   (make-sparse-keymap) "DAP debug-launch commands (d prefix).")
(defvar mhs-dap-e-map   (make-sparse-keymap) "DAP eval/expression commands (e prefix).")

;; Attach sub-maps
(define-key mhs-dap-map "s" mhs-dap-s-map)
(define-key mhs-dap-map "b" mhs-dap-b-map)
(define-key mhs-dap-map "d" mhs-dap-d-map)
(define-key mhs-dap-map "e" mhs-dap-e-map)

;; Direct stepping / control
(define-key mhs-dap-map "n" 'dap-next)
(define-key mhs-dap-map "i" 'dap-step-in)
(define-key mhs-dap-map "o" 'dap-step-out)
(define-key mhs-dap-map "c" 'dap-continue)
(define-key mhs-dap-map "r" 'dap-restart-frame)
(define-key mhs-dap-map "Q" 'dap-disconnect)

;; Session / stack  (s*)
(define-key mhs-dap-s-map "s" 'dap-switch-session)
(define-key mhs-dap-s-map "t" 'dap-switch-thread)
(define-key mhs-dap-s-map "f" 'dap-switch-stack-frame)
(define-key mhs-dap-s-map "u" 'dap-up-stack-frame)
(define-key mhs-dap-s-map "d" 'dap-down-stack-frame)
(define-key mhs-dap-s-map "l" 'dap-ui-locals)
(define-key mhs-dap-s-map "b" 'dap-ui-breakpoints)
(define-key mhs-dap-s-map "S" 'dap-ui-sessions)

;; Breakpoints  (b*)
(define-key mhs-dap-b-map "b" 'dap-breakpoint-toggle)
(define-key mhs-dap-b-map "a" 'dap-breakpoint-add)
(define-key mhs-dap-b-map "d" 'dap-breakpoint-delete)
(define-key mhs-dap-b-map "c" 'dap-breakpoint-condition)
(define-key mhs-dap-b-map "h" 'dap-breakpoint-hit-condition)
(define-key mhs-dap-b-map "l" 'dap-breakpoint-log-message)

;; Debug launch  (d*)
(define-key mhs-dap-d-map "d" 'dap-debug)
(define-key mhs-dap-d-map "r" 'dap-debug-recent)
(define-key mhs-dap-d-map "s" 'dap-debug-restart)
(define-key mhs-dap-d-map "l" 'dap-debug-last)
(define-key mhs-dap-d-map "e" 'dap-debug-edit-template)

;; Eval / expressions  (e*)
(define-key mhs-dap-e-map "e" 'dap-eval)
(define-key mhs-dap-e-map "a" 'dap-ui-expressions-add)
(define-key mhs-dap-e-map "r" 'dap-eval-region)
(define-key mhs-dap-e-map "s" 'dap-eval-thing-at-point)

(global-set-key (kbd "C-c C-d") mhs-dap-map)

(provide 'mhs-dap)
;;; mhs-dap.el ends here
