;; This file is the default file that is loaded when emacs is started
;;--------------------------------------------------------------------


;; Set the BASE of the emacs file locations.
(defvar emacs-top (getenv "EMACS_HOME")
  "this is the top level directory where all of the emacs customizations will live under.
I generally set the EMACS_HOME environmental variable before starting and this is picked up.
Normally this points to: $HOME/.emacs.d/")

;; Read the emacs home directly out of the .profile file. This is a workaround
;; on Darwin systems to allow us to continue
(when (not emacs-top)
  (setq emacs-top (shell-command-to-string "source $HOME/.profile && printf $EMACS_HOME")))

(add-to-list 'load-path emacs-top)

;; Custom Settings
;; ===============
;; To avoid any trouble with the customization system of GNU emacs
;; we set the default file ~/.gnu-emacs-custom
(setq custom-file (concat emacs-top ".gnu-emacs-custom"))
(load custom-file t t)

;; TODO [MHS, 2012-10-03] Remove to an environments file?

;; Check to see if we're running Darwin
(defvar running-macos
  (string-match "darwin" (prin1-to-string system-type))
  "Boolean to determine if we are running on a macintosh laptop" )

;; Sometimes we don't want all of our options.
(defvar running-on-dev-vm
  (string-match "icebadger" (prin1-to-string system-name)))


;; TODO [MHS, 2012-10-03] put this somewhere else
;; Browser settings
(setq browse-url-browser-function (if running-macos
                                      (quote browse-url-default-macosx-browser)
                                      (quote browse-url-firefox))
      browse-url-firefox-new-window-is-tab t
      browse-url-generic-program "firefox"
      browse-url-new-window-flag t
      browse-url-new-window-p t
      browse-url-of-file-hook (quote (browse-url-generic-reload)))


(when running-macos
  (if (file-readable-p (concat emacs-top '"emacs-darwin.el"))
      (load (concat emacs-top '"emacs-darwin.el") nil t)))



;; Attempt to load a feature/library, But don't bail out of the load if it's
;; not around, go ahead and report it to the message buffer.
;-----------------------------------------------------------
;; Set up a list of packages that weren't loaded for multiple machine set-up.
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))

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
