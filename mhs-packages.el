;;; MHS-PACKAGES.EL --- Define my el-get packages and recipes

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
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
     (goto-char (point-max))
     (eval-print-last-sexp))))


(push '(:name yasnippet
              :website "https://github.com/capitaomorte/yasnippet.git"
              :description "YASnippet is a template system for Emacs."
              :type github
              :pkgname "capitaomorte/yasnippet"
              :features "yasnippet"
              :compile "yasnippet.el")
      el-get-sources)

; Also install these packages, no configuration required
(setq my-el-get-packages
      (append
       '(el-get
        ace-jump-mode
        bbdb
        clojure-mode
        auto-complete
        multiple-cursors
        mark-multiple
        ncl-mode
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


; Check packages and install any that are missing
(el-get 'sync my-el-get-packages)
