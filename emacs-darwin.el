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
(when running-macos
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "NODE_PATH"))

;;; .EMACS-DARWIN.EL ends here
