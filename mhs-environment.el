

;; Check to see if we're running Darwin
(defvar running-macos
  (or (string-match "darwin" (prin1-to-string system-type))
      (memq (window-system) '(mac ns)))
  "Boolean to determine if we are running on a macintosh laptop" )



;; Sometimes we don't want all of our options.
(defvar running-on-dev-vm
  (string-match "\\(icebadger\\|savoie\\)" (prin1-to-string system-name)))



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
