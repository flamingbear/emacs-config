;;; BCR_PYTHON.EL --- 
;; Brandon Craig Rhodes's Emacs initialization
;; (Or, at least, the excerpts from it relevant to using the
;;  Python packages Rope, Ropemacs, Pymacs, and PyFlakes.)
;;
;; 2011-05-10 Modified by MHS to work on multiple virtual env installs.

(add-to-list 'load-path (expand-file-name (concat emacs-top '"python-mode/")))

;; Couldn't get completion working quickly.
;; (add-to-list 'load-path (expand-file-name (concat emacs-top '"/python-mode/completion/")))


(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(require 'pymacs)


;; Load ropemacs, which depends on Pymacs (see above).

(pymacs-load "ropemacs" "rope-")

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



; 2011-08-09: <mhs> I don't think we need this anymore, if we activate before
; starting emacs. </mhs> 
; (load-library "virtualenv")

(provide 'bcr-python)
;;; BCR_PYTHON.EL ends here
