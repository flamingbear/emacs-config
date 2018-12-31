;; This sets up fancy modelines on non-terminal displays.


(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'wave)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (setq powerline-image-apple-rgb t)
  (diminish 'auto-revert-mode)

  ;; If you want the leftmost to match your custom faces, use this.
  ;; (defface mhs-spaceline-highlight-face
  ;;   `((t . (:background ,my-modeline-fg-color :foreground ,my-modeline-bg-color :inherit 'mode-line)))
  ;;   "Default highlight face for spaceline.")
  ;; (defun mhs-spaceline-highlight-face-default ()
  ;;   "The default highlight face function.
  ;;      Set `spaceline-highlight-face-func' to
  ;;      `mhs-spaceline-highlight-face-default' to use this."
  ;;   'mhs-spaceline-highlight-face)
  ;; (setq spaceline-highlight-face-func 'mhs-spaceline-highlight-face-default)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  )

(provide 'emacs-modeline)
