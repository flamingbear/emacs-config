;;; .EMACS-DARWIN.EL --- Emacs customizations for Darwin only
;; Copyright (C) 2011 Matt Savoie
;; Author: Matt Savoie <emacs@flamingbear.com>
;; Created: 21 Sep 2011
;; Version: 1.0
;; Keywords:

;;; Code:

(defconst emacs-darwin-version (substring "$Revision: 19643 $" 11 -2)
  "$Id: emacs-darwin.el 19643 2011-10-29 21:07:05Z savoie $
Report bugs to: Matt Savoie <emacs@flamingbear.com>")

;; from exec-path-from-shell

(require 'exec-path-from-shell)
;; TODO [MHS, 2012-10-26] Here's where I'm getting my /usr/bin/python back
;; exec-path-from-shell-initialize before it's set, after it's not.
(when running-macos
  (exec-path-from-shell-copy-env "NODE_PATH")
  (exec-path-from-shell-copy-env "GIT_EDITOR")
  (exec-path-from-shell-copy-env "VIRTUAL_ENV")
  (exec-path-from-shell-initialize))

;; magit bug with using /usr/bin/emacsclient
(eval-after-load 'magit
  '(set-variable 'magit-emacsclient-executable (getenv "GIT_EDITOR")))


;;; .EMACS-DARWIN.EL ends here
