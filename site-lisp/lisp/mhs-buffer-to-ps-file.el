;;; MHS-BUFFER-TO-PS-FILE.EL --- Quickly reformat code into printable files suitable for code review

;; Copyright (C) 2010 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 16 Dec 2010
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
;; mhs-buffer-to-ps-file|Matt Savoie|<emacs@flamingbear.com>
;; |Quickly reformat code into printable files suitable for code review
;; |$Date: 2011-09-18 10:23:58 -0600 (Sun, 18 Sep 2011) $|$Revision: 19387 $|~/packages/mhs-buffer-to-ps-file.el

;;; Commentary:

;;; Change log:
;;

;;; Code:

(defconst mhs-buffer-to-ps-file-version (substring "$Revision: 19387 $" 11 -2)
  "$Id: mhs-buffer-to-ps-file.el 19387 2011-09-18 16:23:58Z savoie $

Report bugs to: Matt Savoie <emacs@flamingbear.com>")

(defun mhs-buffer-to-ps-file ()
  "Called from inside a buffer, will create a new file with buffername.ps suitable for printing"
  (interactive)

 ;  (let ((mhs-out-ps-file (concat (file-name-sans-extension (buffer-file-name)) ".ps"))
  (let ((mhs-out-ps-file (concat (buffer-file-name) ".ps"))
        (ps-print-color-p t)
        (ps-header-lines 1)
        (ps-n-up-printing 1)
        (ps-printer-name "hp4700")
        (ps-line-number t)
        (mhs-face-foreground (face-foreground 'default))
        (mhs-face-background (face-background 'default)))
    ;; This was when I was defining it.
    ;;        (frame-background-mode 'light))

    ;; I really want only black and white when printing....
    (set-face-foreground 'default "black")
    (set-face-background 'default "white")

    ;; Send buffer to output file with the above vars set.
    (ps-print-buffer-with-faces mhs-out-ps-file)

    (set-face-foreground 'default mhs-face-foreground)
    (set-face-background 'default mhs-face-background)))

(provide 'mhs-buffer-to-ps-file)

;;; MHS-BUFFER-TO-PS-FILE.EL ends here
