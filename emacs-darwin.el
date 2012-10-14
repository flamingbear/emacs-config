;;; .EMACS-DARWIN.EL --- Emacs customizations for Darwin only
;; Copyright (C) 2011 Matt Savoie
;; Author: Matt Savoie <emacs@flamingbear.com>
;; Created: 21 Sep 2011
;; Version: 1.0
;; Keywords:

;;; Code:

(defconst emacs-darwin-version (substring "$Revision: 19643 $" 11 -2)
  "$Id: emacs-darwin.el 19643 2011-10-29 21:07:05Z savoie $
Report bugs to: Matt Savoie <savoie@nsidc.org>")

(when running-macos
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (setenv "NODE_PATH" (concat (getenv "HOME") "/node_modules"))
  (push "/opt/local/bin" exec-path))

;;; .EMACS-DARWIN.EL ends here
