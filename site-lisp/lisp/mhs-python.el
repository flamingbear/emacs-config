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
  (setenv "WORKON_HOME" "/Users/savoie/miniconda3/envs")
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))
;; now in your .dir-locals.el put => ((nil . ((pyvenv-workon . "environmentname"))))

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


;; Stole this:  Might be useful if we go to pytest.
;; https://github.com/cryptomaniac512/.emacs.d/blob/master/conf/plugins.el
;; (use-package pytest
;;     :config
;;   (add-to-list 'pytest-project-root-files "pytest.ini")
;;   (setq pytest-cmd-flags "-p no:sugar")
;;   :bind (("C-c C-t C-a" . pytest-all)
;; 	 ("C-c C-t C-m" . pytest-module)
;; 	 ("C-c C-t C-o" . pytest-one)
;; 	 ("C-c C-t C-d" . pytest-directory)
;; 	 ("C-c C-t C-p C-a" . pytest-pdb-all)
;; 	 ("C-c C-t C-p C-m" . pytest-pdb-module)
;; 	 ("C-c C-t C-p C-o" . pytest-pdb-one)))


(use-package ein
  :ensure t
  :init
  (setq ein:completion-backend 'ein:use-company-backend)
  :config
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  )


(provide 'mhs-python)
;;; mhs-python.el ends here
