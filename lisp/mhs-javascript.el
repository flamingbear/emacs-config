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
  (add-hook 'js-mode-hook
            (lambda () (flymake-mode t))))




;; Use linum-mode in javascript
(add-hook 'js-mode-hook (lambda () (linum-mode)))

(provide 'mhs-javascript)

;;; MHS-JAVASCRIPT.EL ends here
