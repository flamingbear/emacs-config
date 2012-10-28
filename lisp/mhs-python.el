;;
;;  This stated as Brandon Craig Rhodes's Emacs initialization for python
;; But was changed with time and looking at this post:
;; http://www.kurup.org/blog/2012/10/24/emacs-for-python-programming/
;;
;;

(setenv "PYMACS_DIR" mhs-virtualenv-dir)
(setenv "PYMACS_PYTHON" (concat (getenv "PYMACS_DIR") "/bin/python"))
(setq py-use-current-dir-when-execute-p t)
(setq py-set-complete-keymap-p t)
(require 'python-mode) ;; TODO [MHS, 2012-10-28]  currently using unreleased from bzr repo
(virtualenv-activate mhs-virtualenv-dir)


;; Set up Flymake to use PyFlakes.

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (concat (getenv "PYMACS_DIR") "/bin/pyflakes") (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)



(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)


;; pink anything over 90 characters
(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

(add-hook 'python-mode-hook (lambda () (linum-mode)))



(provide 'mhs-python)
