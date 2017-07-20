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

(defconst mhs-javascript-version "0.2.0"
  "Report bugs to: Matt Savoie <emacs@flamingbear.com>.")

(use-package js2-mode
  :ensure t
  :mode ("\\.jsx?\\'" . js2-jsx-mode)
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js-indent-level 2))

;; Declaring bankrupcy on my javascript mode until I start working in JS again.
(provide 'mhs-javascript)

;;; MHS-JAVASCRIPT.EL ends here
