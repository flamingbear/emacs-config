;;; .EMACS-DARWIN.EL --- Emacs customizations for Darwin only 

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 21 Sep 2011
;; Version: 1.0
;; Keywords:

;;; Code:

(defconst emacs-darwin-version (substring "$Revision: 19643 $" 11 -2)
  "$Id: emacs-darwin.el 19643 2011-10-29 21:07:05Z savoie $
Report bugs to: Matt Savoie <savoie@nsidc.org>")

;;  [MHS, 2012-06-28] This was what you needed to do before version 24 of
;;  emacs. and to keep the kill-ring and pasteboard separate.
;; TODO [MHS, 2012-07-02]  After further testing, this is needed?  
  ;; This was pulled directly from www.emacswiki.org/emacs/CopyAndPaste
(when (< emacs-major-version 24)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil)
  (defun paste-from-pasteboard ()
    (interactive)
    (and mark-active (filter-buffer-substring (region-beginning) (region-end) t))
    (insert (ns-get-pasteboard))
    )
  (defun copy-to-pasteboard (p1 p2)
    (interactive "r*")
    (ns-set-pasteboard (buffer-substring p1 p2))
    (message "Copied selection to pasteboard")
    )

  (defun cut-to-pasteboard (p1 p2) (interactive "r*") (ns-set-pasteboard (filter-buffer-substring p1 p2 t)) )
  (global-set-key (kbd "s-v") 'paste-from-pasteboard)
  (global-set-key (kbd "s-c") 'copy-to-pasteboard)
  (global-set-key (kbd "s-x") 'cut-to-pasteboard)
  )


(when running-macos
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (setenv "NODE_PATH" (concat (getenv "HOME") "/node_modules"))
  (push "/opt/local/bin" exec-path))


;;; .EMACS-DARWIN.EL ends here
