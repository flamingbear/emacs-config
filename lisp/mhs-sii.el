;;; MHS-SII.EL --- This is a simple set of shortcuts for NRTSIG/SII

;; Copyright (C) 2009 Matt Savoie

;; Author: Matt Savoie <emacs@flamingbear.com>
;; Maintainer: Matt Savoie <emacs@flamingbear.com>
;; Created: 07 Aug 2009
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
;; mhs-sii|Matt Savoie|<emacs@flamingbear.com>
;; |This is a simple set of shortcuts that I need to move around efficiently in the SII.  I'm forever jumping around between directories and it's always dependent on the ${BUILD} environment.
;; |$Date: 2011-10-02 14:24:28 -0600 (Sun, 02 Oct 2011) $|$Revision: 19509 $|~/packages/mhs-sii.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-sii-version (substring "$Revision: 19509 $" 11 -2)
  "$Id: mhs-sii.el 19509 2011-10-02 20:24:28Z savoie $

Report bugs to: Matt Savoie <emacs@flamingbear.com>")


(defvar sii-current-build (getenv "BUILD")
  "Currently active build")

(defvar sii-current-proj-top (getenv "NRTSI_PROJ_TOP")
  "Current Project Top Dir")

;; Functions to set the environment.
(defun df17 ()
  "Set the build to F17 development"
  (interactive)
  (setenv "BUILD" "F17_dev" )
  (setenv "NRTSI_PROJ_TOP" "/projects/NRTSI-G/F17_dev" )
  (setenv "NRTSI_WEBBASE" "/projects/NRTSI-G/F17_dev/website_base")
  (setq sii-current-build (getenv "BUILD"))
  (setq sii-current-proj-top (getenv "NRTSI_PROJ_TOP"))
  (setq sii-current-web (getenv "NRTSI_WEBBASE")))

(defun tf17 ()
  "Set the build to F17 testing"
  (interactive)
  (setenv "BUILD" "F17_test" )
  (setenv "NRTSI_PROJ_TOP" "/projects/NRTSI-G/F17_test" )
  (setenv "NRTSI_WEBBASE" "/disks/testsnowtest/live/data/seaice_index/")
  (setq sii-current-build (getenv "BUILD"))
  (setq sii-current-proj-top (getenv "NRTSI_PROJ_TOP"))
  (setq sii-current-web (getenv "NRTSI_WEBBASE")))

(defun pf17 ()
  "Set the build to F17 production"
  (interactive)
  (setenv "BUILD" "F17_prod" )
  (setenv "NRTSI_PROJ_TOP" "/projects/NRTSI-G/F17_prod" )
  (setenv "NRTSI_WEBBASE" "/disks/production/live/data/seaice_index/")
  (setq sii-current-build (getenv "BUILD"))
  (setq sii-current-proj-top (getenv "NRTSI_PROJ_TOP"))
  (setq sii-current-web (getenv "NRTSI_WEBBASE")))



;; Functions for moving around the environment
(defun nrtsi-proj-top ()
  (setq nrtsi-proj-top-dir sii-current-proj-top)
  nrtsi-proj-top-dir )


(defun nrtsitop-subdir ( subdir )
  "Jump to a subdir of the NRTSTITOP"
  (nrtsi-proj-top-subdir (concat "nrtsig" "/" subdir)))


(defun nrtsi-proj-top-subdir (subdir)
  "jump to a subdirecotry of the nrtsi-proj-top"
  (setq dir-to-jump (concat (nrtsi-proj-top) (concat "/" subdir) ))
  (dired dir-to-jump))


(defun mhs-idl ()
  "Jump to the current mhs-idl library"
  (interactive)
  (nrtsi-proj-top-subdir "extra_idl_libs/idl_util"))

(defun sii-src ()
  "Jump to the current mhs-src library"
  (interactive)
  (nrtsi-proj-top-subdir "nrt_seaice/src/scripts"))

(defun sii-regression ()
  "Jump to the current mhs-regression library"
  (interactive)
  (nrtsi-proj-top-subdir "seaice_index/idl/regression"))

(defun sii-regenerate ()
  "Jump to the current mhs-regression library"
  (interactive)
  (nrtsi-proj-top-subdir "seaice_index/idl/one_off_specials/regenerate_data"))

(defun sii-top ()
  "Jump to current project top"
  (interactive)
  (dired (nrtsi-proj-top)))

(defun sii-results ()
  "Jump to the nrtsi results directory"
  (interactive)
  ( nrtsitop-subdir "results" ))

(defun sii-website ()
  "Jump to the nrtsi results directory"
  (interactive)
  (dired sii-current-web))


(defun sii-ancillary ()
  "Jump to the nrtsi results directory"
  (interactive)
  ( nrtsitop-subdir "ancillary" ))

(defun sii-movies ()
  "jump to the movie dir"
  (interactive)
  (nrtsi-proj-top-subdir "seaice_index/idl/animations/movies"))

(defun sii-lib ()
  "Jump to the nrtsi results directory"
  (interactive)
  ( nrtsitop-subdir "lib" ))

(defun sii-logs ()
  "Jump to the nrtsi logs directory"
  (interactive)
  (nrtsitop-subdir "logs" ))

(defun sii-bin ()
  "jump to the nrtsig bin directory"
  (interactive)
  (nrtsitop-subdir "bin"))

(defun sii-comm ()
  "jump to the nrtsig comm directory"
  (interactive)
  (nrtsitop-subdir "comm"))

(defun sii-libs ()
  "jump to library locs"
  (interactive)
  (nrtsi-proj-top-subdir "seaice_index/idl/sii_libs"))

(defun sii-idl ()
  "Jump to the nrtsi IDL directory"
  (interactive)
  ( nrtsi-proj-top-subdir  "seaice_index/idl" ))

(defun sii-perl ()
  "Jump to the nrtsi perl directory"
  (interactive)
  ( nrtsi-proj-top-subdir  "seaice_index/perl" ))

(defun sii-crons ()
  "jump to the nrtsi crons directory"
  (interactive)
  ( nrtsi-proj-top-subdir "nrt_seaice/cron_info"))

(defun sii-nrt ()
  "jump to the nrt_seaice directory"
  (interactive)
  ( nrtsi-proj-top-subdir "nrt_seaice"))

(defun sii-images ()
  "jump to the seaice_index/idl/images  directory"
  (interactive)
  (nrtsi-proj-top-subdir "seaice_index/idl/images"))

(defun sii-extents ()
  "jump to the seaice_index/idl/extent_ts_output  directory"
  (interactive)
  (nrtsi-proj-top-subdir "seaice_index/idl/extent_ts_output"))


(defun sii-build-string ()
  "Insert the string that sets the environment"
  (interactive)
  (insert '"export BUILD=" (getenv "BUILD")) )

(provide 'mhs-sii)



;;; MHS-SII.EL ends here
