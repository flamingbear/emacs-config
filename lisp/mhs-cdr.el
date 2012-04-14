;;; MHS-CDR.EL --- Helper routines for working on the CDR project

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 06 May 2011
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
;; mhs-cdr|Matt Savoie|<savoie@nsidc.org>
;; |Helper routines for working on the CDR project
;; |$Date$|$Revision$|~/packages/mhs-cdr.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:
 
(defconst mhs-cdr-version 
  "$Id$

Report bugs to: Matt Savoie <savoie@nsidc.org>")


(defvar cdr-current-build (getenv "BUILD")
  "Current build for the CDR project")

(defun cdr-proj-top ()
  (let (cdr-proj-top-dir)
    (setq build cdr-current-build)
    (setq cdr-proj-top-dir (concat '"/projects/cdr/savoie/" build))
    cdr-proj-top-dir))

(defun dcdr ()
  "Set the Build to cdr development"
  (interactive)
  (setenv "BUILD" "cdr_dev")
  (setq cdr-current-build "cdr_dev"))

(defun tcdr ()
  "Set the Build to cdr development"
  (interactive)
  (setenv "BUILD" "cdr_test")
  (setq cdr-current-build "cdr_test"))

(defun proj-top-subdir (subdir)
  "jump to a subdirectory of the project top"
  (let (dir-to-jump)
    (setq dir-to-jump (concat (cdr-proj-top) (concat "/" subdir)))
    dir-to-jump))

(defun cdr-top ()
  "go to the top level CDR dir for the current build"
  (interactive)
  (dired (cdr-proj-top)))

(defun cdr-src ()
  "go to the src directory of the current build"
  (interactive)
  (dired (proj-top-subdir "src")))

(defun cdr-output ()
  "go to the src directory of the current build"
  (interactive)
  (dired (proj-top-subdir "output")))

(defun cdr-work ()
  "go to the work directory of the current build"
  (interactive)
  (dired (proj-top-subdir "work")))


(defun cdr-ancillary ()
  "go to the ancillary directory of the current build"
  (interactive)
  (dired (proj-top-subdir "ancillary")))

(defun cdr-config ()
  "go to the config directory of the current build"
  (interactive)
  (dired (proj-top-subdir "config")))


(defun cdr-test ()
  "go to the test directory of the current build"
  (interactive)
  (dired (proj-top-subdir "test_data")))

(defun cdr-externals ()
  "go to the externals directory of the current build"
  (interactive)
  (dired (proj-top-subdir "externals")))

(provide 'mhs-cdr)

;;; MHS-CDR.EL ends here
