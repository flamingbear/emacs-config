;;; init.el --- staring up emacs since 18.58          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Matt Savoie

;; Author: Matt Savoie <savoie@flamingbear.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  This is my long and crazy emacs setup.  I have stolen plenty of it from other places.
;;  particularly.
;;  magnars: https://github.com/magnars/.emacs.d
;;  kwbeam: https://github.com/kwbeam/kwb-emacs

;;; Code:
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; use cask/pallet to set up and track external packages from gnu/melpa/melpa-stable
(require 'cask (expand-file-name "~/.cask/cask.el"))
(cask-initialize)
(require 'pallet)
(pallet-mode 't)

;; ** Custom Settings that are updated via << M-x customize >>
;; ** Generally Try to avoid putting things in here and prefer setting
;; directly in your init files.
(setq custom-file (locate-user-emacs-file ".gnu-emacs-custom"))
(load custom-file t t)


;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))



;; Add all site-lisp subdir projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))


;; Store "Private" variables/files that don't get checked into revision control
;; github-tokens etc in here.
(defvar mhs-private-dir (locate-user-emacs-file "private"))
(when (file-exists-p mhs-private-dir)
  (add-to-list 'load-path mhs-private-dir)
  (require 'mhs-private-vars))


;; Want backups in a separate directory under the user's dir
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Still make backups even if vc.
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; couple of tweaks for browsers and handling emacs on mac osx
(require 'mhs-environment)


;; Projectile is the BOMB!
(projectile-global-mode)


(require 'emacs-extras)
(require 'emacs-keybinds)
(require 'emacs-sketchy-extras)
(require 'emacs-custom-faces)
(require 'mhs-cut-and-paste)


;; If we found some packages that didn't load..Print them out.
(if missing-packages-list
    (progn (message "Packages not found: %S" missing-packages-list)))
(put 'dired-find-alternate-file 'disabled nil)



;; Work around for bug in macosx
(when running-macos
  (cd (getenv "HOME")))
