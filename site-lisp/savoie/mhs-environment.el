;; Set up some variables that you will use for different environments.
"Boolean to determine if we are running on a macintosh laptop"
(setq running-macos
  (or (equal 'darwin system-type)
      (memq (window-system) '(mac ns))))



;; Sometimes we don't want all of our options.
(defvar running-on-dev-vm
  (string-match "\\(icebadger\\|savoie$\\)" (prin1-to-string system-name)))


;; Browser settings
(setq browse-url-browser-function (if running-macos
                                      (quote browse-url-default-macosx-browser)
                                    (quote browse-url-firefox))

      browse-url-firefox-new-window-is-tab t
      browse-url-generic-program "firefox"
      browse-url-new-window-flag t
      browse-url-new-window-p t
      browse-url-of-file-hook (quote (browse-url-generic-reload)))



;; Attempt to load a feature/library, But don't bail out of the load if it's
;; not around, go ahead and report it to the message buffer.
;-----------------------------------------------------------
;; Set up a list of packages that weren't loaded for multiple machine set-up.
(defvar missing-packages-list nil
  "List of packages that `require' can't find.")


;; Load emacs specials for mac osx only
(when running-macos
  (require 'emacs-darwin))

(provide 'mhs-environment)
;;; mhs-environment.el ends here
