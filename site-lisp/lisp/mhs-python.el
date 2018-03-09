;; Below: Stolen from KWB totally^H^H^H^H^H mostly (well, originally anyway).

;; We're gonna need us a Python mode
(use-package python
  :config
  ;; Python is a dev mode
  (add-hook 'python-mode-hook 'run-dev-hook))

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

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
              (local-set-key "\C-cp." 'nosetests-pdb-one)))

  ;; Added this so that full path to nosetests would work.  This just replaces
  ;; nosetests with full/path/nosetests.  I don't know why it wouldn't work
  ;; before.
  (defun run-nose (&optional tests debug failed)
    "run nosetests"
    (setq nose--last-run-params (list tests debug failed))

    (let* ((nose (executable-find "nosetests"))
	   (where (or nose-local-project-root (nose-find-project-root)))
	   (args (concat (if debug "--pdb" "")
			 " "
			 (if failed "--failed" "")))
	   (tnames (if tests tests "")))
      (if (not where)
	  (error
	   (format (concat "abort: nosemacs couldn't find a project root, "
			   "looked for any of %S") nose-project-root-files)))

      ;; Execute nosetests and display the result in a compilation buffer.
      ;;
      ;; Store the active project root in a buffer-local variable, so that nose
      ;; can invoked from it by the user after execution is complete. This is
      ;; necessary because the compilation buffer doesn't have a filename from
      ;; which it could be discovered.
      (funcall (if debug
		   'pdb
		 '(lambda (command)
		    (let ((compilation-error-regexp-alist
			   '(("  File \"\\(.*\\)\", line \\([0-9]+\\), in test_" 1 2))))
		      (save-current-buffer
			(set-buffer (compilation-start command
						       nil
						       (lambda (mode) (concat "*nosetests*"))))
			(setq-local nose-local-project-root where)))))
	       (format
		(concat "%s "
			(if nose-use-verbose "-v " "")
			"%s -w %s -c %ssetup.cfg %s")
		nose args where where tnames)))
    )
  )


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
