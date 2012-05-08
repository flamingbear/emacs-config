;;; MHS-ACADIS.EL --- Set of directory jumping routines

;; Copyright (C) 2012 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 07 May 2012
;; Version: 1.0
;; Keywords:

;; LCD Archive Entry:
;; mhs-acadis|Matt Savoie|<savoie@nsidc.org>
;; |Set of directory jumping routines
;; |$Date$|$Revision$|~/packages/mhs-acadis.el

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst mhs-acadis-version 
  "$Id$

Report bugs to: Matt Savoie <savoie@nsidc.org>")

(defun dacadis ()
  "Set the Build environment"
  (interactive)
  (setenv "ACADIS_TOPDIR" "/Users/savoie/workspace/acadis/acadis_search/"))

(defun acadis-proj-top ()
  (let (acadis-proj-top-dir)
    (setq acadis-proj-top-dir (getenv "ACADIS_TOPDIR"))
    acadis-proj-top-dir))

(defun acadis-subdir (subdir)
  (let (dir-to-jump)
    (setq dir-to-jump (concat (acadis-proj-top) (concat "/" subdir)))
    dir-to-jump))

(defun acadis-top ()
  "Jump to acadis spec directory"
  (interactive)
  (dired (acadis-subdir "")))

(defun acadis-spec ()
  "Jump to acadis spec directory"
  (interactive)
  (dired (acadis-subdir "/src/test/spec/nsidc/acadis_search/")))

(defun acadis-src ()
  "Jump to acadis source directory"
  (interactive)
  (dired (acadis-subdir "src/scripts/nsidc/acadis_search/")))

(provide 'mhs-acadis)

;;; MHS-ACADIS.EL ends here
