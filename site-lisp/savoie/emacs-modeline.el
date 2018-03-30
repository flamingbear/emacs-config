;; This sets up fancy modelines on non-terminal displays.

(use-package all-the-icons :ensure t)
(use-package spaceline :ensure t)
(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config
  (if (> (display-color-cells) 256)
      (progn
	(spaceline-all-the-icons-theme)
	(set-face-foreground 'mode-line my-modeline-fg-color)
	(set-face-background 'mode-line my-modeline-bg-color)
	(set-face-background 'powerline-active2 my-modeline-bg-color)
	(scroll-bar-mode -1)))

  (defface mhs-spaceline-highlight-face
    `((t . (:background ,my-modeline-bg-color :foreground ,my-modeline-fg-color :inherit 'mode-line)))
    "Default highlight face for spaceline.")

  (defun mhs-spaceline-highlight-face-default ()
    "The default highlight face function.
       Set `spaceline-highlight-face-func' to
       `mhs-spaceline-highlight-face-default' to use this."
    'mhs-spaceline-highlight-face)

  (setq spaceline-highlight-face-func 'mhs-spaceline-highlight-face-default)
  (setq auto-revert-check-vc-info t)
  )


(provide 'emacs-modeline)
