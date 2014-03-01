

;; Below: Stolen from KWB totally

;; We're gonna need us a Python mode
(require 'python)

;; Python is a dev mode
(add-hook 'python-mode-hook 'run-dev-hook)


(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtual_envs/")



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
