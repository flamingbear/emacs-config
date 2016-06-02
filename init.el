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

;;  This is my long and crazy Emacs setup.  I have stolen plenty of it from other places.
;;  particularly.
;;  magnars: https://github.com/magnars/.emacs.d
;;  kwbeam: https://github.com/kwbeam/kwb-emacs

;;; Code:

;; Set init-file and directory location relative to reading this file.  By
;; doing this you can put this emacs config in any directory and start up
;; emacs with:
;; emacs -q -l /path/to/init.el

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


;; ** Custom Settings that are updated via << M-x customize >>
;; ** Generally Try to avoid putting things in here and prefer setting
;; directly in your init files.
(setq custom-file (locate-user-emacs-file ".gnu-emacs-custom"))
(load custom-file t t)


;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))


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

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; couple of tweaks for browsers and handling emacs on mac osx
(require 'mhs-environment)
(require 'emacs-extras)
(require 'emacs-keybinds)
(require 'emacs-sketchy-extras)
(require 'emacs-custom-faces)
(require 'mhs-cut-and-paste)



;; Work around for bug in macosx
(when running-macos
  (cd (getenv "HOME")))
