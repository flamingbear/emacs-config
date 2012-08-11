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
  (setenv "ACADIS_TOPDIR" "/Users/savoie/projects/acadis/acadis_search"))

(defun acadis-proj-top ()
  (let (acadis-proj-top-dir)
    (setq acadis-proj-top-dir (getenv "ACADIS_TOPDIR"))
    (when (eq acadis-proj-top-dir 'nil)
      (dacadis)
      (setq acadis-proj-top-dir (getenv "ACADIS_TOPDIR")) )
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
  (dired (acadis-subdir "src/test/spec/nsidc/acadis_search")))

(defun acadis-src ()
  "Jump to acadis source directory"
  (interactive)
  (dired (acadis-subdir "src/scripts/nsidc/acadis_search")))


(defun acadis-buffer-is-a-spec-file ()
  (interactive)
  (string-match "spec.js"  (buffer-name)))


(defun acadis-jump-to-matching-spec ()
  "jump to the spec file for this js source file"
  (interactive)
  (let ((filename (buffer-file-name))
        (matching-spec))
    (setq matching-spec (acadis-convert-src-to-spec-fn (buffer-file-name)))
    (find-file matching-spec)))


(defun acadis-jump-to-matching-src ()
  "jump to the spec file for this js source file"
  (interactive)
  (let ((filename (buffer-file-name))
        (matching-src))
    (setq matching-src (acadis-convert-spec-to-src-fn (buffer-file-name)))
    (find-file matching-src)))


(defun acadis-convert-spec-to-src-fn (spec-file)
  "Take the input spec file and generate the matching src file."
  (let ((spec-base  (file-name-nondirectory spec-file))
        (src-dir (acadis-subdir "src/scripts/nsidc/acadis_search/"))
        (src-file))
    (setq src-file  (concat (substring spec-base 0 -8) ".js"))
    (concat src-dir src-file)))


(defun acadis-convert-src-to-spec-fn (src-file)
  "change the input filename into a spec filename"
  (interactive) 
  (let ((src-base (file-name-nondirectory src-file))
        (spec-dir (acadis-subdir "src/test/spec/nsidc/acadis_search/"))
        (spec-file))
    (setq spec-file (concat (substring src-base 0 -3) "_spec.js"))
    (concat spec-dir spec-file)))


(defun acadis-jump-to-spec-or-code ()
  "jump to the code, or the spec for the current file"
  (interactive)
  (if (acadis-buffer-is-a-spec-file)
      (acadis-jump-to-matching-src)
    (acadis-jump-to-matching-spec)))

(defun acadis-split-window-and-show-match ()
  "split the current window and open the spec"
  (interactive)
  (split-window)
  (acadis-jump-to-spec-or-code))

(provide 'mhs-acadis)

;;; MHS-ACADIS.EL ends here
