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



;; This was pulled directly from www.emacswiki.org/emacs/CopyAndPaste
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


(when running-macos
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (setenv "NODE_PATH" (concat (getenv "HOME") "/node_modules"))
  (push "/opt/local/bin" exec-path))

;; 2011-10-29: <mhs> This doesn't work yet, but might be a solutionon mac...</mhs>
;; (when (not (getenv "TERM_PROGRAM"))
;;   (setenv "PATH" (shell-command-to-string "source $HOME/.profile && printf $PATH" ))
;;   (setenv "PATH" (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))
;; (getenv "PATH")
;; http://www.emacswiki.org/emacs/EmacsApp#toc2
;;     (setenv "PATH" (shell-command-to-string "source $HOME/.profile && printf $PATH" ))
;; also paste-from-pasteboard

;;; .EMACS-DARWIN.EL ends here
