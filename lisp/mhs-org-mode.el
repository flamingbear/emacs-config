;;; MHS-ORG-MODE.EL --- Commands for ORG Mode operation.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 13 May 2011
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
;; mhs-org-mode|Matt Savoie|<savoie@nsidc.org>
;; |Commands for ORG Mode operation.
;; |$Date$|$Revision: 19387 $|~/packages/mhs-org-mode.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-org-mode-version (substring "$Revision: 19387 $" 11 -2)
   "$Id$ Report bugs to: Matt Savoie <savoie@nsidc.org>")
;; ORG MODE information
;;----------------------
(require 'org-protocol)

;; Keep a clock across working sessions.
(org-clock-persistence-insinuate)

;; The following lines are always needed.  
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)

(org-remember-insinuate)
(setq org-directory (concat emacs-top '"mhs/orgs/"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Prefer capture to remember.
;(define-key global-map "\C-cr" 'org-remember)
(define-key global-map "\C-cr" 'org-capture)


(provide 'mhs-org-mode)

;;; MHS-ORG-MODE.EL ends here
