;;; MHS-SMARTTABS.EL --- set up for smart tabs

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 06 Oct 2011
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
;; mhs-smarttabs|Matt Savoie|<savoie@nsidc.org>
;; |set up for smart tabs
;; |$Date: 2011-10-06 08:32:40 -0600 (Thu, 06 Oct 2011) $|$Revision: 19548 $|~/packages/mhs-smarttabs.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-smarttabs-version (substring "$Revision: 19548 $" 11 -2)
  "$Id: mhs-smarttabs.el 19548 2011-10-06 14:32:40Z savoie $

Report bugs to: Matt Savoie <savoie@nsidc.org>")


(defvar mhs-smart-tabs-dir (expand-file-name (concat mhs-external-lisp-dir "smart-tabs")))

(when (file-accessible-directory-p mhs-smart-tabs-dir)
  (add-to-list 'load-path mhs-smart-tabs-dir))

(when (and (try-require 'smart-tabs-mode)
           (try-require 'cperl-mode))
  (smart-tabs-advice cperl-indent-line cperl-indent-level))

(provide 'mhs-smarttabs)
;;; MHS-SMARTTABS.EL ends here
