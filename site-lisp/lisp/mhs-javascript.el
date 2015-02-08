;;; MHS-JAVASCRIPT.EL --- Tweaks for Javascript coding in Emacs.

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 09 Mar 2012
;; Version: 1.0
;; Keywords:

;;; Commentary:

;;; Change log:
;;

;;; Code:

(defconst mhs-javascript-version "0.1.0"
  "$Id$

Report bugs to: Matt Savoie <emacs@flamingbear.com>")

;; [MHS, 2012-08-11] I found flymake-node-jshint when trying to fix the old
;; jshint-mode and this works with node-jshint and allows configuration via a
;; .json file It requires you to install node-jshint first

;; Load a .jshintrc.json file as the default settings to jshint  This search
;; (defun mhs-set-flymake-node-jshint-config-file ()
;;   "Search upward in directories looking for a .jshintrc.json file and set it to be the file used in flymake-node-jshint"
;;   (interactive)
;;   (let* ((current-loc (buffer-file-name))
;;          (jshintrc-name ".jshintrc.json")
;;          (path-to-file (locate-dominating-file current-loc jshintrc-name)))
;;     (when path-to-file
;;       (setq flymake-node-jshint-config  (expand-file-name (concat path-to-file jshintrc-name))))))


;; (when (try-require 'flymake-node-jshint)
;;   (setq flymake-node-jshint-config "/Users/savoie/.jshintrc.json") ; optional
;;   (add-hook 'js-mode-hook (lambda ()
;;                             (mhs-set-flymake-node-jshint-config-file)
;;                             (flymake-mode 1)))
;;   (add-hook 'js2-mode-hook (lambda ()
;;                              (mhs-set-flymake-node-jshint-config-file)
;;                              (flymake-mode 1))))



;; Use linum-mode in javascript
(add-hook 'js2-mode-hook
          (lambda ()
            (linum-mode)
            (tern-mode t)
            (local-set-key [(control meta q)] 'prog-indent-sexp) ))

(autoload 'js2-mode "js2-mode" nil t)

(custom-set-variables
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p nil)
 '(js2-global-externs '("define" "module" "require"
                        "jQuery" "$" "_" "buster" "sinon"
                        "assert" "refute"
                        "setTimeout" "clearTimeout"
                        "setInterval" "clearInterval"
                        "location" "__dirname" "console" "JSON"
                        "require" "chai" "expect" "exports" "before" "after"
                        "describe" "beforeEach" "afterEach" "it")))





(when (try-require 'js-comint)
  ;; Use node as our repl
  (setq inferior-js-program-command "node")

  (setq inferior-js-mode-hook
        (lambda ()
          ;; We like nice colors
          (ansi-color-for-comint-mode-on)
          ;; Deal with some prompt nonsense
          (add-to-list 'comint-preoutput-filter-functions
                       (lambda (output)
                         (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                                                   (replace-regexp-in-string ".*1G.*3G" "node> " output)))) ))

                                        ;  (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
  (add-hook 'js2-mode-hook '(lambda ()
                              (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                              (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                              (local-set-key "\C-cb" 'js-send-buffer)
                              (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                              (local-set-key "\C-cl" 'js-load-file-and-go)
                              )))


(provide 'mhs-javascript)

;;; MHS-JAVASCRIPT.EL ends here
