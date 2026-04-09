;;; mhs-dap.el --- DAP (Debug Adapter Protocol) configuration  -*- lexical-binding: t; -*-

;; Unified dap-mode config for Python and Node.js debugging.

(use-package dap-mode
  :ensure t
  :defer t
  :hook
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :commands dap-debug
  :config
  (setq dap-auto-configure-features '(sessions repl tooltip breakpoints))

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
   "Attach to Harmony"
   (list :type "node"
         :request "attach"
         :port 9200
         :address "127.0.0.1"
         :restart t
         :sourceMaps t
         :name "Attach to Harmony")))

(provide 'mhs-dap)
;;; mhs-dap.el ends here
