;;; MHS-PRINT.EL --- Collection of routines that are useful when postscript printing from emacs

;; Copyright (C) 2007 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 04 Oct 2007
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
;; program's author (send electronic mail to <emacs@flamingbear.com>) or from the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; mhs-print|Matt Savoie|<emacs@flamingbear.com>
;; |Collection of routines that are useful when postscript printing from emacs
;; |$Date: 2011-09-18 10:23:58 -0600 (Sun, 18 Sep 2011) $|$Revision: 19387 $|~/packages/mhs-print.el

;;; Commentary:

;;; Change log:
;; $Log: elisp-insert.el,v $
;; Revision 1.1.1.1  2002/02/28 18:00:54  savoie
;; imported sources
;;

;;; Code:

(defconst mhs-print-version (substring "$Revision: 19387 $" 11 -2)
  "$Id: mhs-print.el 19387 2011-09-18 16:23:58Z savoie $

Report bugs to: Matt Savoie <emacs@flamingbear.com>")


;; All I really want is a way to set a bunch of variables for the session.
;; And then unset them afterwards.

;; 2007-10-25: <mhs> But for now you can call this to get it ready for action.</mhs>

(defun mhs-file-to-ps-file ()
  "Called from inside a buffer, will create a new file with buffername.ps suitable for printing"
  (interactive)
  ;; This is probably not the lispy way to do this, but I'm hacking.  Maybe with a let?
  (let ((mhs-out-ps-file (concat (file-name-sans-extension (buffer-file-name)) ".ps"))
        ;;(frame-background-mode 'light)
        (ps-print-color-p t)
        (ps-header-lines 1)
        (ps-n-up-printing 1)
        (ps-printer-name "hp4700")
        (ps-line-number t)
        (mhs-face-foreground (face-foreground 'default))
        (mhs-face-background (face-background 'default)))

    (set-face-foreground 'default "black")
    (set-face-background 'default "white")

    ;; Send buffer to output file with the above vars set.
    (ps-print-buffer-with-faces mhs-out-ps-file)

    (set-face-foreground 'default mhs-face-foreground)
    (set-face-background 'default mhs-face-background) ))



;;; MHS-PRINT.EL ends here
