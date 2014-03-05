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
; https://bigmac.caelum.uberspace.de/paste/ensure-package-installed.html
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every package not installed."
  (mapcar
   (lambda (package)
     (package-installed-p 'evil)
     (if (package-installed-p package)
         package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         nil)))
   packages))


; install the missing packages
(dolist (package mhs-packages)
  (unless (package-installed-p package)
    (ensure-package-installed package)))
