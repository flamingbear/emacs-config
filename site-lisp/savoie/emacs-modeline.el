;; This sets up fancy modelines on non-terminal displays.


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-vcs-max-length 55)
  (setq doom-modeline-window-width-limit nil)
  )

(provide 'emacs-modeline)
