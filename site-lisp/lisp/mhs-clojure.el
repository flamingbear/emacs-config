;;; MHS-CLOJURE.EL ---

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 30 Jun 2012
;; Version: 1.0
;; Keywords:


;;; Commentary:  Just my stuff for setting up clojure

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-clojure-version "0.0.1"
  "Report bugs to: Matt Savoie <mattsemacs@flamingbear.com>")

;; TODO [MHS, 2012-07-01] Need to load swank first or this won't work.
(when (and (try-require 'clojure-mode)
           (try-require 'clojure-test-mode)
           (try-require 'paredit))
(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup)))))

(provide 'mhs-clojure)


;;; MHS-CLOJURE.EL ends here
