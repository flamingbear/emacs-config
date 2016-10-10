;;; mhs-cut-and-paste.el ---                         -*- lexical-binding: t; -*-

;; I found this online to help with copy paste across terminal/X /osx/ubuntu

;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/


;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
;; (setq x-select-enable-clipboard t)

;; Turn on pbcopy if you're running emacs on a mac regardless of the window type.
;;--------------------------------------------------------------------------------

(if (equal 'darwin system-type)
    (use-package pbcopy
      :ensure t
      :config
      (turn-on-pbcopy)))

;; use this fancy thing I found if you're in a terminal, but not on macosx
;;-------------------------------------------------------------------------

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(setq select-enable-clipboard t)
(unless (or window-system (equal 'darwin system-type))
 (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--primary" "--input"))
    ;; My debugging
    ;; (message "Called xsel-cut-function")
    ;; (sleep-for .15)
    ;; (message (shell-command-to-string "xsel --primary --output"))
    )

  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --primary --output")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output ))
    ;; MHS debugging.
    ;; (message "Called xsel-paste-function")
    )
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
 ))


(provide 'mhs-cut-and-paste)
;;; mhs-cut-and-paste.el ends here
