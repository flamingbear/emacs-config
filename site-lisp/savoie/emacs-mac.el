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
  (dolist (var '("GIT_EDITOR" "EDITOR" "LC_ALL" "NVM_DIR" "AWS_PROFILE"
                 "AWS_SDK_LOAD_CONFIG" "WORKON_HOME" "CC" "LIBRARY_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (setq exec-path-from-shell-shell-name "zsh")
  (setq exec-path-from-shell-arguments nil) ;; nil = non-interactive, reads .zshenv only = fast
  (exec-path-from-shell-initialize)

  ;; Trying this here from running into jit issues You might add these to the above? If this doesn't work,
  ;; you  an set CC and LIBRARY_PATH and run emacs from a terminal to pick everything up.
  ;; export CC=gcc-14
  ;; export LIBRARY_PATH="$LIBRARY_PATH:/opt/homebrew/opt/gcc/lib/gcc/current:/opt/homebrew/lib/gcc/current"
  ;; (setenv "CC" "gcc-15")
  ;; (setenv "LIBRARY_PATH"
  ;;         (concat (getenv "LIBRARY_PATH")
  ;;                 ":/opt/homebrew/opt/gcc/lib/gcc/current:/opt/homebrew/lib/gcc/current"))
  )


(provide 'emacs-mac)
;;; emacs-mac.el ends here
