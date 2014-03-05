;;; MHS-PACKAGES.EL --- Define my el-get packages and recipes

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 14 Oct 2012
;; Version: 1.0
;; Keywords:

;;; Code:

;; Set up package locations
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))


(when (not package-archive-contents)
  (package-refresh-contents))


(defvar mhs-packages
  '(ace-jump-mode
    auto-complete
    autopair
    clojure-mode
    edit-server
    feature-mode
    ein
    w3m
    exec-path-from-shell
    expand-region
    flycheck
    git-gutter-fringe
    idlwave
    jedi
    magit
    mark-multiple
    markdown-mode
    multiple-cursors
    nose
    nrepl
    paredit
    psvn
    python
    rinari
    skewer-mode
    virtualenvwrapper
    wgrep
    yaml-mode
    yasnippet))


; prompt before installing
; install the missing packages
(dolist (package mhs-packages)
  (unless (package-installed-p package)
    (if (y-or-n-p (format "Package %s is missing. Install it? " package))
        (package-install package))))

(provide 'mhs-packages)
;;; mhs-packages.el ends here
