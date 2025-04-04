;;; .EMACS-MAC.EL --- Emacs customizations for Mac only
;; Copyright (C) 2011 Matt Savoie
;; Author: Matt Savoie <emacs@flamingbear.com>
;; Created: 21 Sep 2011
;; Version: 1.0
;; Keywords:
;;; Commentary:
;;; Code:
(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))


(use-package exec-path-from-shell
  :ensure t
  :config
  (append-to-list 'exec-path-from-shell-variables
		  '("GIT_EDITOR" "EDITOR" "LC_ALL" "NVM_DIR" "AWS_PROFILE" "AWS_SDK_LOAD_CONFIG" "WORKON_HOME"))
  (setq exec-path-from-shell-shell-name "zsh")
  (setq exec-path-from-shell-arguments '("-l" "-i")) ;; Use login shell but not interactive
  (exec-path-from-shell-initialize)
  )

;; If you run into jit issues You might add these to the above?
;; export CC=gcc-14
;; export LIBRARY_PATH="$LIBRARY_PATH:/opt/homebrew/opt/gcc/lib/gcc/current:/opt/homebrew/lib/gcc/current"


(provide 'emacs-mac)
;;; emacs-mac.el ends here
