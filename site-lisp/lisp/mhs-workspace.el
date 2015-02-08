;;; MHS-WORKSPACE.EL --- Easily set and jump to a workspace.

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 15 Jun 2012
;; Version: 1.0
;; Keywords:

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-workspace-version "0.0.1"
"Report bugs to: Matt Savoie <emacs@flamingbear.com>")

(defvar mhs-current-workspace  (convert-standard-filename (concat (getenv "HOME") "/workspace/"))
  "Current working location" )


(defun mhs-set-workspace ()
  (interactive)
  (setq mhs-current-workspace default-directory))

(defun mhs-jump-workspace ()
  (interactive)
  (dired mhs-current-workspace))

(defun mhs-workspace-action (&optional arg)
  "Either set (with prefix) or jump to the current workspace"
  (interactive "P")
  (if current-prefix-arg
      (mhs-set-workspace)
    (mhs-jump-workspace)))

(provide 'mhs-workspace)

;;; MHS-WORKSPACE.EL ends here
