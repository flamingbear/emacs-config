;;; MHS-MAIL.EL --- Handles mailto command from opera

;; Copyright (C) 2002 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 27 Feb 2002
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
;; mhs-mail|Matt Savoie|<savoie@nsidc.org>
;; |Handles mailto command from opera
;; |$Date: 2011-09-18 10:23:58 -0600 (Sun, 18 Sep 2011) $|$Revision: 19387 $|~/packages/mhs-mail.el

;;; Commentary:

;;; Change log:
;; $Log: not supported by cvs2svn $
;; Revision 1.1.1.1  2002/02/28 18:00:54  savoie
;; imported sources
;;
;; Revision 1.1  2002/02/27 16:59:02  savoie
;; Initial revision
;;

;;; Code:

(defconst mhs-mail-version (substring "$Revision: 19387 $" 11 -2)
  "$Id: mhs-mail.el 19387 2011-09-18 16:23:58Z savoie $

Report bugs to: Matt Savoie <savoie@nsidc.org>")

(require 'gnus-msg)

(defun dxr-group-mail (To Subject)
  "Start composing a mail."
  (interactive)
  (gnus-setup-message 'message
    (message-mail To Subject)))

(defun mhs-mailto (&optional to cc subject body)
(select-frame (selected-frame))
(raise-frame)
(dxr-group-mail to subject)
(insert body)
)

(provide 'mhs-mail)

;;; MHS-MAIL.EL ends here
