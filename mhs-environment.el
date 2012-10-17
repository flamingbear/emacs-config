

;; Check to see if we're running Darwin
(defvar running-macos
  (string-match "darwin" (prin1-to-string system-type))
  "Boolean to determine if we are running on a macintosh laptop" )

;; Sometimes we don't want all of our options.
(defvar running-on-dev-vm
  (string-match "icebadger" (prin1-to-string system-name)))



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
