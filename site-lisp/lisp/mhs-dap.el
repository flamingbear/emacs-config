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
         :name "Node :: Attach by PID")))

(provide 'mhs-dap)
;;; mhs-dap.el ends here

;; launch.json?
;; {
;;   "version": "0.2.0",
;;   "configurations": [
;;     {
;;       "type": "node",
;;       "request": "launch",
;;       "name": "Debug Harmony Service",
;;       "skipFiles": ["<node_internals>/**"],
;;       "program": "${workspaceFolder}/services/harmony/app/server.ts", // Adjust if your entry point is different
;;       "runtimeArgs": ["-r", "ts-node/register"],
;;       "envFile": "${workspaceFolder}/services/harmony/.env", // Loads your environment variables
;;       "sourceMaps": true,
;;       "smartStep": true,
;;       "console": "integratedTerminal",
;;       "outFiles": ["${workspaceFolder}/services/harmony/dist/**/*.js"]
;;     },
;;     {
;;       "type": "node",
;;       "request": "attach",
;;       "name": "Attach to Process",
;;       "port": 9229,
;;       "skipFiles": ["<node_internals>/**"]
;;     }
;;   ]
;; }
;; launch json for emacs?
;; {
;;   "version": "0.2.0",
;;   "configurations": [
;;     {
;;       "type": "node",
;;       "request": "launch",
;;       "name": "Harmony: Debug Locator Middleware",
;;       "skipFiles": ["<node_internals>/**"],
;;       "program": "${workspaceFolder}/services/harmony/app/server.ts",
;;       "runtimeArgs": [
;;         "-r", "ts-node/register"
;;       ],
;;       "args": [],
;;       "cwd": "${workspaceFolder}/services/harmony",
;;       "envFile": "${workspaceFolder}/services/harmony/.env",
;;       "protocol": "inspector",
;;       "console": "integratedTerminal",
;;       "sourceMaps": true
;;     }
;;   ]
;; }
