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


(defvar sii-proj-top (getenv "NRTSI_PROJ_TOP") "Current Project Top Dir.")

(defvar sii-output-root (getenv "OUTPUT_ROOT") "Output_root location.")

(defun sii-set-topdir ()
  (interactive)
  (setq sii-proj-top (file-name-directory (buffer-file-name))))


(defun nrtsi-proj-top ()
  "Functions for moving around the environment."
  sii-proj-top )


(defun nrtsi-out-top ()
  "Functions for moving around the environment."
  sii-output-root )


(defun nrtsi-proj-top-subdir ( subdir )
  "Jump to a SUBDIR of the NRTSTI-PROJ-TOP."
  (concat (nrtsi-proj-top) "/" subdir))

(defun nrtsi-out-subdir ( subdir )
  "Jump to a SUBDIR of the OUTPUT_ROOT."
  (concat (nrtsi-out-top) "/" subdir))

(defun sii-mhs-idl ()
  "Jump to the current mhs-idl library."
  (interactive)
  (dired (nrtsi-proj-top-subdir "extra_idl_libs/idl_util")))

(defun sii-top ()
  "Jump to current project top."
  (interactive)
  (dired (nrtsi-proj-top)))

(defun sii-website ()
  "Jump to the nrtsi results directory."
  (interactive)
  (dired (nrtsi-out-subdir "website")))


(defun sii-ancillary ()
  "Jump to the nrtsi results directory."
  (interactive)
  (dired  (nrtsi-out-subdir "nrtsig/ancillary" )))

(defun sii-movies ()
  "Jump to the movie dir."
  (interactive)
  (dired (nrtsi-proj-top-subdir "/seaice_index/idl/animations/movies")))

(defun sii-lib ()
  "Jump to the nrtsi results directory."
  (interactive)
  (dired (nrtsi-out-subdir "nrtsig/lib" )))

(defun sii-logs ()
  "Jump to the nrtsi logs directory."
  (interactive)
  (dired (nrtsi-out-subdir "nrtsig/logs" )))


(defun sii-libs ()
  "Jump to library locs."
  (interactive)
  (dired (nrtsi-proj-top-subdir "seaice_index/idl/sii_libs")))

(defun sii-idl ()
  "Jump to the nrtsi IDL directory."
  (interactive)
  (dired ( nrtsi-proj-top-subdir  "seaice_index/idl" )))

(defun sii-perl ()
  "Jump to the nrtsi perl directory."
  (interactive)
  (dired ( nrtsi-proj-top-subdir  "seaice_index/perl" )))

(defun sii-images ()
  "Jump to the images directory."
  (interactive)
  (dired (nrtsi-proj-top-subdir "seaice_index/idl/images")))

(defun sii-extents ()
  "Jump to the extent_ts_output directory."
  (interactive)
  (dired (nrtsi-out-subdir "extent_ts_output")))


(provide 'mhs-sii)



;;; MHS-SII.EL ends here
