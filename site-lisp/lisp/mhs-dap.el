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
         :name "Attach to Harmony"))

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
         :name "Node :: Attach to Test (9229)"))

  (global-set-key (kbd "<f10>")    'dap-hydra) ; F10   ;; ORYX Debug Layer - Y
  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (add-hook 'post-command-hook
                        (lambda ()
                          (when (fboundp 'dap-hydra/nil)
                            (dap-hydra/nil)))
                        nil t))))

(provide 'mhs-dap)
;;; mhs-dap.el ends here
