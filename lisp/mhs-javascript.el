;;; MHS-JAVASCRIPT.EL --- Tweaks for Javascript coding in Emacs.

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 09 Mar 2012
;; Version: 1.0
;; Keywords:

;;; Commentary:

;;; Change log:
;;

;;; Code:

(defconst mhs-javascript-version "0.1.0"
  "$Id$

Report bugs to: Matt Savoie <savoie@nsidc.org>")

;; [MHS, 2012-08-11] I found this trying to fix the old jshint-mode and this
;; works with node-jshint and allows configuration.
;; Requires you to install node-jshint first
(defun mhs-search-project-for-jshintrc ()
  (interactive)
   (let* ((current-loc (buffer-file-name))
          (jshintrc-name ".jshintrc.json")
          (path-to-file (locate-dominating-file current-loc jshintrc-name)))
     (when path-to-file
       (setq flymake-node-jshint-config  (expand-file-name (concat path-to-file jshintrc-name))))))


(when (try-require 'flymake-node-jshint)
  (setq flymake-node-jshint-config "/Users/savoie/.jshintrc.json") ; optional
  (add-hook 'js-mode-hook (lambda ()
                            (mhs-search-project-for-jshintrc)
                            (flymake-mode 1)))
  (add-hook 'js2-mode-hook (lambda ()
                             (mhs-search-project-for-jshintrc)
                             (flymake-mode 1))))


;; Use linum-mode in javascript
(add-hook 'js-mode-hook (lambda () (linum-mode)))
(add-hook 'js2-mode-hook
          (lambda ()
            (linum-mode)
            (local-set-key [(control meta q)] 'prog-indent-sexp) ))

(autoload 'js2-mode "js2-mode" nil t)
(setq js2-basic-offset 2)

;(add-to-list 'auto-mode-alist '("\\.js" . js2-mode) nil)



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
                              )) )



(provide 'mhs-javascript)

;;; MHS-JAVASCRIPT.EL ends here
