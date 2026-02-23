;;; init.el --- starting up emacs since 18.58          -*- lexical-binding: t; -*-

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

;; Don't garbage collect on startup and use a larger than default GC threshold
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package diminish :ensure t)
(require 'bind-key)

;; time your emacs init
;; call benchmark-init/show-durations-tree or benchmark-init/show-durations-tabulated
(use-package benchmark-init :ensure t)


;; ** Custom Settings that are updated via << M-x customize >>
;; ** Generally Try to avoid putting things in here and prefer setting
;; directly in your init files.
(setq custom-file (locate-user-emacs-file ".gnu-emacs-custom"))
(load custom-file t t)


;; Authorization information stored in gpg encrypted file.
(use-package auth-source
  :config
  (setq
   auth-sources `(,(expand-file-name "private/.authinfo.gpg" user-emacs-directory)
		  )
   auth-source-debug 'nil))

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))


;; Add all site-lisp subdir projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))


;; Want backups in a separate directory under the user's dir
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(use-package saveplace
  :config
  (save-place-mode 't)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))


;; Size for extended large monitor
;; personal laptop = 1512
;; work laptop = 1792
;; work laptop + external monitor = 51xx?
;; debug with: (frame-parameter nil 'height)
(if (> (display-pixel-width) 5000)
    (setq default-frame-alist
	  '((height . 75)
            (width . 210)
            (left . 600)
            (top . 175)
            (horizontal-scroll-bars . nil)
            (tool-bar-lines . 0)))
  ;; Size for m1 13" laptop
  (setq default-frame-alist
	'((height . 41)
          (width . 130)
          (left . 113)
          (top . 43)
          (horizontal-scroll-bars . nil)
          (tool-bar-lines . 0)))
  )


(use-package mhs-environment)
(use-package emacs-extras)
(use-package emacs-keybinds)
(use-package emacs-ministack)
(use-package mhs-scrum)
(use-package mhs-org-mode)
(use-package mhs-org-clock-into)
(use-package emacs-custom-faces)
(use-package emacs-modeline)
(use-package emacs-gpt)


;; Work around for bug in macosx
(when running-macos
  (cd (getenv "HOME")))
