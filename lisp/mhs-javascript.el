;;; MHS-JAVASCRIPT.EL --- Tweaks for Javascript coding in Emacs.

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 09 Mar 2012
;; Version: 1.0
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <savoie@nsidc.org>) or from the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; mhs-javascript|Matt Savoie|<savoie@nsidc.org>
;; |Tweaks for Javascript coding in Emacs.
;; |$Date$|$Revision$|~/packages/mhs-javascript.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-javascript-version "0.1.0"
  "$Id$

Report bugs to: Matt Savoie <savoie@nsidc.org>")



;; JS HINT stuff
(when (try-require 'flymake-jshint)
  (add-hook 'js2-mode-hook
            (lambda () (flymake-mode t)))
  (add-hook 'js-mode-hook
            (lambda () (flymake-mode t))) )


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
                                                    (replace-regexp-in-string ".*1G.*3G" "node>" output)))) ))

;  (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
  (add-hook 'js2-mode-hook '(lambda () 
                              (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                              (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                              (local-set-key "\C-cb" 'js-send-buffer)
                              (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                              (local-set-key "\C-cl" 'js-load-file-and-go)
                              )) )


(defvar mhs-jshint-lines
  (list nil
        "/*jshint forin:true, noarg:true, noempty:true, white:false, maxlen:160, indent:2, eqeqeq:true, bitwise:true, undef:true, curly:true, browser:true, onevar:true, immed:true, newcap:true */
/*global nsidc: true, jQuery, $, _, Backbone, it, expect, describe, beforeEach, afterEach, sinon, debug, runs, waitsFor, waits */
"
)
  "*A string to insert at the top of javascript files to make jshint work" )

(defun mhs-insert-jshint-lines ()
  "Add text to the top of a js file"
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (insert (car (cdr mhs-jshint-lines)))))


(defun mhs-remove-jshint-lines ()
  "Remove the jshint information fromt the top of a javascript file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (car (cdr mhs-jshint-lines)) nil t)
      (replace-match "" t t))))


(provide 'mhs-javascript)

;;; MHS-JAVASCRIPT.EL ends here
