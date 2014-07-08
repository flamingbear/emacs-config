;; This file is the default file that is loaded when emacs is started It
;; should set up environment and then load additional packages as necessary
;; -------------------------------------------------------------------------

;; Set the BASE of the emacs directory structure and add it to my load-path
(defvar emacs-top (getenv "EMACS_HOME")
  "this is the top level directory where all of the emacs customizations will live under.
I generally set the EMACS_HOME environmental variable before starting and this is picked up.
Normally this points to: $HOME/.emacs.d/")

(when (not emacs-top)
  ;; If you didn't have it set before, try to read directly out of the
  ;; .profile file. This is a workaround on Darwin systems to allow us to
  ;; continue
  (setq emacs-top (shell-command-to-string "source $HOME/.profile && printf $EMACS_HOME")))

(setq emacs-top (file-name-as-directory emacs-top))
(add-to-list 'load-path emacs-top)

(require 'cask "/usr/local/Cellar/cask/0.7.0/cask.el")
(cask-initialize)
(require 'pallet)


;; ** Custom Settings that are updated via << M-x customize >> **
(setq custom-file (concat emacs-top ".gnu-emacs-custom"))
(load custom-file t t)

;; Private variables that don't get checked into revision control.
(defvar mhs-private-dir (concat (file-name-as-directory emacs-top) "private"))
(when (file-exists-p mhs-private-dir)
  (add-to-list 'load-path mhs-private-dir)
  (require 'mhs-private-vars))



;; Want backups in a separate directory under emacs-top
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat (file-name-directory emacs-top) "backups")))))


;; Extra Dired commands
(add-hook 'dired-load-hook
          (function (lambda ()
                      (load "dired-x")
                      ;; Set global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      )))

;; couple of tweaks for browsers and handling emacs on mac osx
(load "mhs-environment" t t)

(when running-macos
  (if (file-readable-p (concat emacs-top '"emacs-darwin.el"))
      (load (concat emacs-top '"emacs-darwin.el") nil t)))


(autoload 'skewer-start "setup-skewer" nil t)

(projectile-global-mode)



;; add load paths to custom files, load special packages, load the
;; mhs-idlwave-extras file.
(if (file-readable-p (concat emacs-top '"emacs-extras.el"))
    (load (concat emacs-top '"emacs-extras.el") nil t))


;; My Settings for keybinds/maps
(if (file-readable-p (concat emacs-top '"emacs-keybinds.el"))
    (load (concat emacs-top '"emacs-keybinds.el") nil t))



;; These are things that are sometimes tricky...
(if (file-readable-p (concat emacs-top '"emacs-sketchy-extras.el"))
    (load (concat emacs-top '"emacs-sketchy-extras.el")))



;; load custom faces in separate file.
(if (file-readable-p (concat emacs-top '"emacs-custom-faces.el"))
    (load (concat emacs-top '"emacs-custom-faces.el") nil t))


;; If we found some packages that didn't load..Print them out.
(if missing-packages-list
    (progn (message "Packages not found: %S" missing-packages-list)))
(put 'dired-find-alternate-file 'disabled nil)


;; colorize compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Work around for bug in macosx
(when running-macos
  (cd (getenv "HOME")))
