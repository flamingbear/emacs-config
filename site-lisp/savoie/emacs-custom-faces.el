;; Add custom themes
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(add-to-list 'custom-theme-load-path custom-theme-directory)

;; We like to know what machine we're running on.  So change the color
;; depending on where you're running from. Also set defaults like what idl
;; command to run for specific versions.
(use-package rainbow-mode :ensure t)
(defvar my-modeline-fg-color "#ffffff"
  "My foreground color for the modeline should be customized below.")
(defvar my-modeline-bg-color "#a52a2a"
  "My background color for the modeline should be customized below.")


(when (display-graphic-p)
  ;; If we're not running NX set the fonts like I like them.  Otherwise, we
  ;; default to 10x20
  (cond (running-macos (progn (mhs-use-inconsolata)))
        (t (mhs-use-normal-face))))


(defun mhs-update-mode-line ()
  "Set the mode and menu bar colors according to customizations."
  (interactive)
  ;; Set both mode and menu rather than inheriting
  (set-face-foreground 'mode-line my-modeline-fg-color)
  (set-face-background 'mode-line my-modeline-bg-color)

  (set-face-foreground 'menu my-modeline-fg-color)
  (set-face-background 'menu my-modeline-bg-color)

  (set-face-foreground 'vertical-border my-modeline-fg-color)
  (set-face-background 'vertical-border my-modeline-bg-color)
  )

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t     ; if nil, bold is universally disabled
        doom-themes-enable-italic t)  ; if nil, italics is universally disabled

  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-nord t)
  (load-theme 'modus-vivendi-tinted t)
  (doom-themes-visual-bell-config)

  (doom-themes-org-config)
  (set-face-attribute 'org-headline-done nil :foreground "#207030")
  )

;; all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(provide 'emacs-custom-faces)
;;; emacs-custom-faces.el ends here
