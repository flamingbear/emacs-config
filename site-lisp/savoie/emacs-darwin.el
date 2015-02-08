;;; .EMACS-DARWIN.EL --- Emacs customizations for Darwin only
;; Copyright (C) 2011 Matt Savoie
;; Author: Matt Savoie <emacs@flamingbear.com>
;; Created: 21 Sep 2011
;; Version: 1.0
;; Keywords:
;;; Commentary:
;;; Code:



(require 'exec-path-from-shell)
;; TODO [MHS, 2012-10-26] Here's where I'm getting my /usr/bin/python back
;; exec-path-from-shell-initialize before it's set, after it's not.
(when running-macos
  (exec-path-from-shell-copy-env "NODE_PATH")
  (exec-path-from-shell-copy-env "GIT_EDITOR")
  (exec-path-from-shell-copy-env "VIRTUAL_ENV")
  (exec-path-from-shell-initialize))

;; These two below both preport to fixes bad cut/paste in osx for emacs 23.3
;; "Quit: "empty or unsupported pasteboard type""
(when (eq window-system 'ns)
  (defadvice ns-get-pasteboard (around hack-empty-pasteboard compile activate)
    (condition-case err
        ad-do-it
      (quit (message "%s" (cadr err))
            nil))))

(setq save-interprogram-paste-before-kill nil)



;; magit bug with using /usr/bin/emacsclient
(eval-after-load 'magit
  '(set-variable 'magit-emacsclient-executable (getenv "GIT_EDITOR")))


(provide 'emacs-darwin)
;;; emacs-darwin.el ends here
