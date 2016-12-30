;; Below: Stolen from KWB totally^H^H^H^H^H mostly.

;; We're gonna need us a Python mode
(use-package python
  :config
  ;; Python is a dev mode
  (add-hook 'python-mode-hook 'run-dev-hook))

(use-package pyvenv
  :ensure t
  :config
  ;; track virtual environments if they are set dir locally
  (setq pyvenv-tracking-mode 't))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))




;; don't use flymake (elpy default), use flycheck
;; https://github.com/jorgenschaefer/elpy/issues/137#issuecomment-55403160
(use-package flycheck
  :ensure t
  :config
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode))



(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "/Users/savoie/anaconda2/envs")
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))
;; now in your .dirp-locals.el put => ((nil . ((pyvenv-workon . "environmentname"))))

;; Be able to run nose tests with various keybindings
(use-package nose
  :ensure t
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key "\C-ca" 'nosetests-all)
              (local-set-key "\C-cm" 'nosetests-module)
              (local-set-key "\C-c." 'nosetests-one)
              (local-set-key "\C-cpa" 'nosetests-pdb-all)
              (local-set-key "\C-cpm" 'nosetests-pdb-module)
              (local-set-key "\C-cp." 'nosetests-pdb-one))))



;;
;;; Special configuration for ein:notebook autocomplete
;;;
;;; This is still a problem.
(use-package auto-complete  :ensure t)

(use-package ein
  :ensure t
  :config
  (setq ein:use-auto-complete t)
  (setq ein:use-auto-complete-superpack t)
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  (add-hook 'ein:notebook-mode-hook 'mhs-ein-notebook-hook)

  (defun mhs-ein-notebook-hook ()
    (interactive)
    (require 'auto-complete-config nil t)
    (declare-function auto-complete-mode "auto-complete.el")
    (when (featurep 'auto-complete-config)
      (company-mode -1)
      (company-quickhelp-mode -1)
      ;; ein hangs if garbage collection is too small.  Make it Yoooge!
      ;; (setq gc-cons-threshold 100000000)
      (auto-complete-mode t))))


(provide 'mhs-python)
;;; mhs-python.el ends here
