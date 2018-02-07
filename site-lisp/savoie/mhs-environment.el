;; Set up some variables that you will use for different environments.

;; "Boolean to determine if we are running on a macintosh laptop"
(setq running-macos
  (or (equal 'darwin system-type)
      (memq (window-system) '(mac ns))))


;; Browser settings
(setq browse-url-browser-function (if running-macos
                                      (quote browse-url-default-macosx-browser)
                                    (quote browse-url-firefox))
      browse-url-firefox-new-window-is-tab t
      browse-url-generic-program "firefox"
      browse-url-new-window-flag t
      browse-url-new-window-p t
      browse-url-of-file-hook (quote (browse-url-generic-reload)))



;; Load emacs specials for mac osx only
(when running-macos
  (use-package emacs-mac))

(provide 'mhs-environment)
;;; mhs-environment.el ends here
