;;; MHS-MASIE.EL --- quick movements for masie project

;; Copyright (C) 2010 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 12 Aug 2010
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
;; mhs-masie|Matt Savoie|<emacs@flamingbear.com>
;; |quick movements for masie project
;; |$Date: 2011-10-23 18:34:01 -0600 (Sun, 23 Oct 2011) $|$Revision: 19617 $|~/packages/mhs-masie.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-masie-version (substring "$Revision: 19617 $" 11 -2)
  "$Id: mhs-masie.el 19617 2011-10-24 00:34:01Z savoie $

Report bugs to: Matt Savoie <emacs@flamingbear.com>")


(defvar masie-current-build (getenv "BUILD")
  "current build for the masie project")


(defun masie-func-error ()
  "Insert the proper function for masie error handling"
  (interactive)
  (insert "Catch,  theError
   if theError ne 0 then begin
      Catch,  /CANCEL
      masie_log,  /is_error
      return,
   endif "))

;; Functions to set the environment.
(defun dmasie ()
  "Set the build to MASIE development"
  (interactive)
  (setenv "BUILD" "MASIE_dev" )
  (setq masie-current-build "MASIE_dev")
  (setenv "MASIE_TOP" "/projects/MASIE/MASIE_dev" ))

(defun tmasie ()
  "Set the build to MASIE development"
  (interactive)
  (setenv "BUILD" "MASIE_test" )
  (setq masie-current-build "MASIE_test")
  (setenv "MASIE_TOP" "/projects/MASIE/MASIE_test" ))

(defun masie-proj-top ()
  "fetch the previously set MASIE_TOP environment variable"
  (setq masie-top (concat '"/projects/MASIE/" masie-current-build))
  masie-top)

(defun masie-subdir (subdir)
  "jump to a sudir of the masie top directory"
  (setq dir-to-jump (concat (masie-proj-top) (concat "/" subdir)))
  (dired dir-to-jump))

(defun masie-top ()
  "Go to the top level masie directory"
  (interactive)
  (setq dir-to-jump (masie-proj-top))
  (dired dir-to-jump))

(defun masie-src ()
  (interactive)
  (masie-subdir "src"))

(defun masie-uber ()
  (interactive)
  (masie-subdir "uber_build"))

(defun masie-masks ()
  (interactive)
  (masie-subdir "masks"))

(defun masie-environment ()
  (interactive)
  (masie-subdir "environment"))


(defun masie-output ()
  (interactive)
  (masie-subdir "output"))

(defun masie-plots ()
  (interactive)
  (masie-subdir "output/plots"))

(defun masie-logs ()
  (interactive)
  (masie-subdir "logs"))

(defun masie-ancillary ()
  (interactive)
  (masie-subdir "ancillary"))


(provide 'mhs-masie)

;;; MHS-MASIE.EL ends here
