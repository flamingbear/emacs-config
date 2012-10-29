;;
;;  This stated as Brandon Craig Rhodes's Emacs initialization for python
;; But was changed with time and looking at this post:
;; http://www.kurup.org/blog/2012/10/24/emacs-for-python-programming/
;;


;; autocomplete
(require 'auto-complete-config)
(setq ac-dictionary-files (list (concat user-emacs-directory ".dict")))
(ac-config-default)


(setenv "PYMACS_DIR" mhs-virtualenv-dir)
(setenv "PYMACS_PYTHON" (concat (getenv "PYMACS_DIR") "/bin/python"))

;; hack to fix ac-sources after pycomplete.el breaks it
(add-hook 'python-mode-hook
          '(lambda ()
             (setq ac-sources '(ac-source-pycomplete
                                ac-source-abbrev
                                ac-source-dictionary
                                ac-source-words-in-same-mode-buffers))))

(setq py-install-directory (concat emacs-top "external-lisp-files/python-mode/"))
(add-to-list 'load-path py-install-directory)

;; show method sigs when typing.
(setq py-set-complete-keymap-p t)
(setq py-use-current-dir-when-execute-p t)

(require 'python-mode) ;; TODO [MHS, 2012-10-28]  currently using unreleased from bzr repo

;; activate your virtual environment
(virtualenv-activate mhs-virtualenv-dir)

(defun load-pycomplete ()
  "Load and initialize pycomplete."
  (interactive)
  (let* ((pyshell (py-choose-shell))
         (path (getenv "PYTHONPATH")))
    (setenv "PYTHONPATH" (concat
                          (expand-file-name py-install-directory) "completion"
                          (if path (concat path-separator path))))
    (if (py-install-directory-check)
        (progn
          (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
                                      "python"
                                    pyshell))
          (autoload 'pymacs-apply "pymacs")
          (autoload 'pymacs-call "pymacs")
          (autoload 'pymacs-eval "pymacs")
          (autoload 'pymacs-exec "pymacs")
          (autoload 'pymacs-load "pymacs")
          (load (concat py-install-directory "completion/pycomplete.el") nil t)
          (add-hook 'python-mode-hook 'py-complete-initialize))
      (error "`py-install-directory' not set, see INSTALL"))))
(eval-after-load 'pymacs '(load-pycomplete))
(eval-after-load 'python-mode '(load-pycomplete))


;; pyflakes flymake integration
;; pycheckers is in my localbin. and is just
;; pyflakes "$1"
;; pep8 --repeat "$1"
;; true
;; http://stackoverflow.com/a/1257306/347942
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook 'flymake-mode)


(add-hook 'find-file-hook 'flymake-find-file-hook)


;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                    interpreter-mode-alist))
;; (autoload 'python-mode "python-mode" "Python editing mode." t)


;; pink anything over 90 characters
(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

(add-hook 'python-mode-hook (lambda () (linum-mode)))



(provide 'mhs-python)
