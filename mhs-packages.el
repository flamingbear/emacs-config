;;; MHS-PACKAGES.EL --- Define my el-get packages and recipes

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 14 Oct 2012
;; Version: 1.0
;; Keywords:

;;; Code:


(setq el-get-dir (concat emacs-top "el-get"))
(setq el-get-install-dir (concat el-get-dir "/el-get"))
(add-to-list 'load-path el-get-install-dir)

;; I was having trouble with the wiki recipes so I skip them.
(setq el-get-install-skip-emacswiki-recipes t)

; If el-get is missing, install it automatically

(unless (require 'el-get nil t)
(url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))


(push '(:name yasnippet
              :website "https://github.com/capitaomorte/yasnippet.git"
              :description "YASnippet is a template system for Emacs."
              :type github
              :pkgname "capitaomorte/yasnippet"
              :features "yasnippet"
              :compile "yasnippet.el")
      el-get-sources)

(push '(:name flymake-node-jshint
              :website "https://github.com/jegbjerg/flymake-node-jshint"
              :description "Emacs library providing simple flymake for JavaScript using JSHint through node-jshint"
              :type github
              :pkgname "jegbjerg/flymake-node-jshint") el-get-sources)

(push '(:name Emacs-wgrep
              :website "https://github.com/mhayashi1120/Emacs-wgrep"
              :description "Writable grep buffer and apply the changes to files "
              :type github
              :pkgname "mhayashi1120/Emacs-wgrep") el-get-sources)

(push '(:name exec-path-from-shell
              :website "https://github.com/purcell/exec-path-from-shell"
              :description "A GNU Emacs library to setup environment variables from the user's shell."
              :type github
              :pkgname "purcell/exec-path-from-shell") el-get-sources)


;; These all moved and aren't updated in eg-get yet.
(push '(:name auto-complete
       :website "https://github.com/auto-complete/auto-complete"
       :description "The most intelligent auto-completion extension."
       :type github
       :pkgname "auto-complete/auto-complete"
       :depends (popup fuzzy)) el-get-sources)

(push '(:name popup
       :website "https://github.com/auto-complete/popup-el"
       :description "Visual Popup Interface Library for Emacs"
       :type github
       :pkgname "auto-complete/popup-el") el-get-sources)

(push '(:name fuzzy
       :website "https://github.com/auto-complete/fuzzy-el"
       :description "Fuzzy matching utilities for GNU Emacs"
       :type github
       :pkgname "auto-complete/fuzzy-el") el-get-sources)



; Also install these packages, no configuration required
(setq my-el-get-packages
      (append '(el-get
                ace-jump-mode
                edit-server
                clojure-mode
                auto-complete
                multiple-cursors
                mark-multiple
                nrepl
                paredit
                psvn
                rinari
                yaml-mode
                markdown-mode
                emacs-w3m
                expand-region
                magit )
              (mapcar 'el-get-source-name el-get-sources)))

(when running-macos
  (push 'bbdb my-el-get-packages ))


; Check packages and install any that are missing
(el-get 'sync my-el-get-packages)
