;;; MAC-ENVIRONMENT.EL --- copy of someone's ideas to fix environment by default.

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 11 Mar 2012
;; Version: 1.0
;; Keywords:


;; Call this code before running the app to update environment., but need to restart too...
;; #!/bin/bash
;; bash -l -c "/Applications/Emacs.app/Contents/MacOS/Emacs --batch -l ~/lib/emacs/elisp/macosx/environment-support.el -f generate-env-plist"
;; Then after you've done this, go delete any SSH key stuff...SSH_AUTH_SOCK

(defconst mac-environment-version "0.1.0"
  "$Id$

Report bugs to: Matt Savoie <savoie@nsidc.org>")


  ;;; Provide support for the environment on Mac OS X
(defun generate-env-plist ()
  "Dump the current environment into the ~/.MacOSX/environment.plist file."
  ;; The system environment is found in the global variable:
  ;; 'initial-environment as a list of "KEY=VALUE" pairs.
  (let ((list initial-environment)
        pair start)
    ;; clear out the current environment settings
    (find-file "~/.MacOSX/environment.plist")
    (goto-char (point-min))
    (setq start (search-forward "<dict>\n"))
    (search-forward "</dict>")
    (beginning-of-line)
    (delete-region start (point))
    (while list
      (setq pair (split-string (car list) "=")
            list (cdr list))
      (when (not (string-match "SSH" (nth 0 pair)))
        (insert "  <key>" (nth 0 pair) "</key>\n")
        (insert "  <string>" (nth 1 pair) "</string>\n")))
    ;; Save the buffer.
    (save-buffer)))


;;; MAC-ENVIRONMENT.EL ends here
