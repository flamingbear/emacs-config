;; Below: Stolen from KWB totally^H^H^H^H^H mostly.

;; We're gonna need us a Python mode
(require 'python)
(require 'elpy)
(require 'pyvenv)
(elpy-enable)

;; don't use flymake (elpy default), use flycheck
;; from: https://github.com/jorgenschaefer/elpy/issues/137
(when (require 'flycheck nil t)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; track virtual environments if they are set dir locally
(setq pyvenv-tracking-mode 't)

;; Python is a dev mode
(add-hook 'python-mode-hook 'run-dev-hook)



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



;;
;;; Special configuration for ein:notebook autocomplete
;;;
;;; This is still a problem.

(require 'ein-loaddefs)
(eval-when-compile (require 'ein-notebooklist))
(require 'ein)

;; auto-complete superpack
;; (setq ein:use-auto-complete-superpack t)

(defun mhs-ein-notebook-hook ()
  (interactive)
  (require 'auto-complete-config nil t)
  (declare-function auto-complete-mode "auto-complete.el")
  (when (featurep 'auto-complete-config)
    (company-mode -1)
    (ac-config-default)
    (auto-complete-mode t)))

; 2015-06-30 This is the one that causes problems when you have large arrays
(add-hook 'ein:notebook-mode-hook 'mhs-ein-notebook-hook)
(add-hook 'ein:notebooklist-first-open-hook 'mhs-ein-notebook-hook)
;; (add-hook 'ein:connect-mode-hook 'mhs-ein-notebook-hook)


(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; https://github.com/millejoh/emacs-ipython-notebook/issues/67#issuecomment-165782847
;; (defun my-ein-dot-function ()
;;   (interactive)
;;   (if (bound-and-true-p auto-complete-mode)
;;       (ein:notebook-complete-dot)
;;     (self-insert-command 1)))           ;problem remains: cannot insert many dots this way...

;; (define-key ein:notebook-mode-map (kbd ".") 'my-ein-dot-function)


(provide 'mhs-python)
;;; mhs-python.el ends here
