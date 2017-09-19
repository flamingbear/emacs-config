;;; .EMACS-DARWIN.EL --- Emacs customizations for Darwin only
;; Copyright (C) 2011 Matt Savoie
;; Author: Matt Savoie <emacs@flamingbear.com>
;; Created: 21 Sep 2011
;; Version: 1.0
;; Keywords:
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell :ensure t
  :config
  (exec-path-from-shell-copy-env "NODE_PATH")
  (exec-path-from-shell-copy-env "GIT_EDITOR")
  (exec-path-from-shell-copy-env "EDITOR")
  (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-initialize)
)

(provide 'emacs-darwin)
;;; emacs-darwin.el ends here
