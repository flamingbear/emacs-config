;; Below: Stolen from KWB totally^H^H^H^H^H mostly.

;; We're gonna need us a Python mode
(require 'python)
(require 'elpy)
(require 'pyvenv)
(elpy-enable)

;; don't use flymake (elpy default), use flycheck
;; from: https://github.com/jorgenschaefer/elpy/issues/137
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; track virtual environments if they are set dir locally
(setq pyvenv-tracking-mode 't)

;; Python is a dev mode
(add-hook 'python-mode-hook 'run-dev-hook)


;; (require 'virtualenvwrapper)
;; (venv-initialize-interactive-shells) ;; if you want interactive shell support
;; (venv-initialize-eshell) ;; if you want eshell support
;; (setq venv-location "~/.virtual_envs/")



;; Be able to run nose tests with various keybindings
(require 'nose)
(add-hook 'python-mode-hook
          (lambda ()
            (hack-local-variables)
            (when (boundp 'project-venv-name)
              (pyvenv-activate project-venv-name))
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one)))


;; the venv-workon above lets you use .dir-local.el files to control the virtual env and activate it...
;; sample .dir-locals.el file for python
;; ((nil . ((venv-location . "/home/savoie/projects/pyswath/.pyswath")))
;;  (python-mode . ((project-venv-name . "local"))))

;; Use the Python force, my young padawan learner
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)


;;
;;; Special configuration for ein:notebook autocomplete
(require 'ein-loaddefs)
(eval-when-compile (require 'ein-notebooklist))
(require 'ein)

  ;; auto-complete superpack
(setq ein:use-auto-complete-superpack t)

(defun mhs-ein-notebook-hook ()
  (interactive)
  (require 'auto-complete-config nil t)
  (declare-function auto-complete-mode "auto-complete.el")
  (when (featurep 'auto-complete-config)
    (company-mode -1)
    (ac-config-default)
    (auto-complete-mode t)))

;(add-hook 'ein:notebooklist-first-open-hook 'mhs-ein-notebook-hook)
(add-hook 'ein:notebook-mode-hook 'mhs-ein-notebook-hook)
;(add-hook 'ein:connect-mode-hook 'mhs-ein-notebook-hook)
;(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(provide 'mhs-python)
;;; mhs-python.el ends here
