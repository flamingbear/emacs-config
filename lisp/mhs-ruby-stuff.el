;;; MHS-RUBY-STUFF.EL --- Quick place to stash all of my ruby loads.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 27 Sep 2011
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
;; mhs-ruby-stuff|Matt Savoie|<savoie@nsidc.org>
;; |Quick place to stash all of my ruby loads.
;; |$Date: 2011-10-23 17:43:02 -0600 (Sun, 23 Oct 2011) $|$Revision: 19616 $|~/packages/mhs-ruby-stuff.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-ruby-stuff-version (substring "$Revision: 19616 $" 11 -2)
  "$Id: mhs-ruby-stuff.el 19616 2011-10-23 23:43:02Z savoie $

Report bugs to: Matt Savoie <savoie@nsidc.org>")



;; RUBY RINARI (RINARI IS NOT A RAILS IDE...not really)
;;-----------------------------------------
(defvar my-rinari-top-dir
  (expand-file-name (concat emacs-top '"external-lisp-files/rinari/")))
(if (file-accessible-directory-p my-rinari-top-dir)
    (add-to-list 'load-path my-rinari-top-dir))
(try-require 'rinari)


;; Ruby autoload for syntax highlighting and keybindings
;;-------------------------------------------------------

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys) ))

;; This is my regex seach for interactive shells, lets you search for anything
;; old commands by regex
(add-hook 'inferior-ruby-mode-hook
          (lambda ()
            (local-set-key [down] 'jds-history-search-down)
            (local-set-key [up] 'jds-history-search-up)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key [(super E)] 'ruby-send-region-and-go)
            (local-set-key [(super e)] 'ruby-send-region)))



;;; nXML % nXHTML (HTML ERB template support)
(when (> emacs-major-version 23)
  (defvar nxml-dir (concat emacs-top "external-lisp-files/nxml/"))
  (when (file-accessible-directory-p nxml-dir)
    (add-to-list 'load-path nxml-dir)))

(when (try-require 'nxml-mode)
  (add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode)))

(defvar nxhtml-dir (concat emacs-top "external-lisp-files/nxhtml/"))


(when (file-accessible-directory-p nxhtml-dir)
  ;;  this nex line causes problems with INFO paths, but it might not be necessary.
  ;; (add-to-list 'load-path nxhtml-dir) ; <- this causes info problems.
  (load (expand-file-name (concat nxhtml-dir "autostart.el")))
  (setq
   nxhtml-global-minor-mode t
   mumamo-chunk-coloring 'submode-colored
   nxhtml-skip-welcome t
   indent-region-mode t
   rng-nxml-auto-validate-flag nil
   nxml-degraded t)
  ;; mumamo-background-chunk-major, - dark blue
  (set-face-background 'mumamo-background-chunk-major "#1C2138")
  ;;mumamo-background-chunk-submode1 - dark green
  (set-face-background 'mumamo-background-chunk-submode1 "#0B3621")
  ;; (add-to-list 'auto-mode-alist '("\\.erb" . eruby-nxhtml-mumamo) )
  )



(provide 'mhs-ruby-stuff)

;;; MHS-RUBY-STUFF.EL ends here
