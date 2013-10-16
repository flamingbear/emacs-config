;;; MHS-PYTHON-2.EL ---

(defun add-dir-to-python-path () "\t
    Add current directory to PYTHONPATH"
  (interactive)
  (setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":" (file-name-directory (buffer-file-name)))))



;; Below: Stolen from KWB totally

;; We're gonna need us a Python mode
(require 'python)

;; Python is a dev mode
(add-hook 'python-mode-hook 'run-dev-hook)

;; All the Python things live here
(setq virtualenv-root "~/.virtual_envs/")

;; flymake & flake8
;; (add-hook 'python-mode-hook 'flymake-mode)
;; (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;; (setq flymake-python-pyflakes-executable "flake8")
;; (require 'flymake-cursor)

;; Be lazy about activating ropemacs
(defun load-ropemacs ()
    "Load pymacs and ropemacs"
    (interactive)
    (require 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    ;; Automatically save project python buffers before refactorings
    (setq ropemacs-confirm-saving 'nil))
(global-set-key "\C-xpl" 'load-ropemacs)

;; Be able to run nose tests with various keybindings
(require 'nose)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one)))

;; Use the Python force, my young padawan learner
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)





(provide 'mhs-python-2)
;;; MHS-PYTHON-2.EL ends here
