;;; thing-cmds.el --- Commands that use things, as defined by `thingatpt.el'.
;; 
;; Filename: thing-cmds.el
;; Description: Commands that use things, as defined by `thingatpt.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2009, Drew Adams, all rights reserved.
;; Created: Sun Jul 30 16:40:29 2006
;; Version: 20.1
;; Last-Updated: Sat Dec 27 10:09:28 2008 (-0800)
;;           By: dradams
;;     Update #: 122
;; URL: http://www.emacswiki.org/cgi-bin/wiki/thing-cmds.el
;; Keywords: thingatpt, thing, region, selection
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;;  You can use the commands defined here to select different kinds of
;;  text entities ("things").  They are especially useful in
;;  combination with Transient Mark mode.
;; 
;;  Commands defined here:
;;
;;    `cycle-thing-region', `mark-thing', `select-thing-near-point',
;;    `thing-region'.
;;
;;  User options defined here:
;;
;;    `thing-types'.
;;
;;  Internal variables defined here:
;;
;;    `mark-thing-type', `thing-region-index'.
;;
;;  Put this in your init file (`~/.emacs'): (require 'thing-cmds)
;;
;;  Suggested key bindings (these will replace the standard bindings
;;  for `mark-sexp' and `mark-word'):
;;
;;  (global-set-key [(control meta ? )] 'mark-thing) ; vs `mark-sexp'
;;  (global-set-key [(meta ?@)] 'cycle-thing-region) ; vs `mark-word'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2008/11/29 dadams
;;     mark-thing: Set point to beginning/end of thing, so whole thing gets marked.
;;                 Make completion default be the first element of thing-types.
;; 2007/07/15 dadams
;;     Added cycle-thing-region-point.
;;     cycle-thing-region: Save point in cycle-thing-region-point and reuse it.
;; 2006/07/30 dadams
;;     Created.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'thingatpt+ nil t) ;; (no error if not found): bounds-of-thing-at-point
(require 'thingatpt) ;; bounds-of-thing-at-point

(when (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

;;;;;;;;;;;;;;;;;;;;;;;;

(defun thing-region (thing)
  "Set the region around a THING near the cursor.
You are prompted for the type of thing.  Completion is available for
some standard types of thing, but you can enter any type.
The cursor is placed at the end of the region.  You can return it to
the original location by using `C-u C-SPC' twice."
  (interactive (list (let ((icicle-sort-function  nil))
                       (completing-read "Type of thing: " (mapcar #'list thing-types)
                                        nil nil nil nil "sexp"))))
  (let* ((bds    (if (fboundp 'bounds-of-thing-nearest-point) ; In `thingatpt+.el'.
                     (bounds-of-thing-nearest-point (intern thing))
                   (bounds-of-thing-at-point (intern thing))))
         (start  (car bds))
         (end    (cdr bds)))
    (cond ((and start end)
           (push-mark (point) t)        ; Mark position, so can use `C-u C-SPC'.
           (goto-char end)
           (push-mark start t 'activate)
           (setq deactivate-mark  nil)
           thing)                       ; Return thing.
          (t
           (message "No `%s' near point" thing)
           (setq deactivate-mark  nil)
           nil))))                      ; Return nil: no thing found.

(defalias 'select-thing-near-point 'cycle-thing-region)
(defun cycle-thing-region ()
  "Select a thing near point.  Successive uses select different things.
The default thing type is the first element of option `thing-types'. 
In Transient Mark mode, you can follow this with `\\[mark-thing]' to select
successive things of the same type, but to do that you must first use
`C-x C-x': `\\[cycle-thing-region] C-x C-x \\[mark-thing]'"
  (interactive)
  (if (eq last-command this-command)
      (goto-char cycle-thing-region-point)
    (setq thing-region-index        0
          cycle-thing-region-point  (point)))
  (let* ((thing    (elt thing-types thing-region-index))
         (success  (thing-region thing)))
    (setq thing-region-index  (1+ thing-region-index))
    (when success
      (setq mark-thing-type  (intern thing)) ; Save it for `mark-thing'.
      (message "%s" (capitalize (elt thing-types (1- thing-region-index)))))
    (when (>= thing-region-index (length thing-types))
      (setq thing-region-index  0))))

(defcustom thing-types '("word" "symbol" "sexp" "list" "line" "sentence"
                         "paragraph" "page" "defun" "number" "form")
  "List of thing types.  Used for completion and `cycle-thing-region'.
Each list element is a string that names a type of text entity for
which there is a either a corresponding `forward-'thing operation, or
corresponding `beginning-of-'thing and `end-of-'thing operations.
Examples include \"word\", \"sentence\", and \"defun\".

The first element is the default thing type used by `mark-thing' and
`cycle-thing-region'."
  :type '(repeat string))

(defvar thing-region-index 0 "Index of current thing in `thing-types'.")

(defvar mark-thing-type nil "Current thing type used by `mark-thing'.")

(defvar cycle-thing-region-point nil
  "Position of point before `cycle-thing-region'.")

(defun mark-thing (thing &optional arg allow-extend)
  "Set point at one end of THING and set mark ARG THINGs from point.
THING is a symbol that names a type of thing.  Interactively, the
symbol name is read: \"word\", \"sexp\", and so on.  See option
`thing-types' for more examples.

Put mark at the same place command `forward-'THING would put it with
the same prefix argument.

Put point at the beginning of THING, unless the prefix argument (ARG)
is negative, in which case put it at the end of THING.

Interactively:

You are prompted for THING.  Completion is available for the types of
thing in user option `thing-types', but you can enter any type.  The
default value is the first element of `thing-types'.

If `mark-thing' is repeated or if the mark is active (in Transient
Mark mode), then it marks the next ARG THINGs, after the ones already
marked.  The type of THING used is whatever was used the last time
`mark-thing' was called.

This region extension reusing the last type of THING happens even if
the active region is empty.  This means that you can, for instance,
just use `C-SPC' to activate an empty region and then use `mark-thing'
to select more THINGS of the last kind selected."
  (interactive "i\nP\np")               ; THING arg is nil (ignored) interactively.
  (let ((this-cmd  this-command)
        (last-cmd  last-command))
    (cond ((and allow-extend (or (and (eq last-cmd this-cmd) (mark t))
                                 (and transient-mark-mode mark-active)))
           (setq arg  (if arg
                          (prefix-numeric-value arg)
                        (if (< (mark) (point)) -1 1)))
           (set-mark (save-excursion
                       (goto-char (mark))
                       (forward-thing mark-thing-type arg)
                       (point))))
          (t
           (setq mark-thing-type
                 (or thing
                     (intern (prog1
                                 (let ((icicle-sort-function  nil))
                                   (completing-read "Type of thing: "
                                                    (mapcar #'list thing-types)
                                                    nil nil nil nil
                                                    (car thing-types)))
                               (setq this-command  this-cmd)))))
           (push-mark (save-excursion
                        (forward-thing mark-thing-type (prefix-numeric-value arg))
                        (point))
                      nil t)))
    (unless (memq this-cmd (list last-cmd 'cycle-thing-region))
      (forward-thing mark-thing-type (if (< (mark) (point)) 1 -1))))
  (setq deactivate-mark  nil))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'thing-cmds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thing-cmds.el ends here
