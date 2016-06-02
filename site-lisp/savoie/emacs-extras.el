;; File for extra emacs commands that are pretty standard, but useful
;;--------------------------------------------------------------------


;; Override having to type Yes or No with just Y or N
(fset 'yes-or-no-p 'y-or-n-p)
(put 'scroll-left 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; To determine when to split horizontally
(setq split-width-threshold '1600)

(ansi-color-for-comint-mode-on)

;; Some magnar's Sane Defaults.
;;------------------------------
;; Show keystrokes in progress
(setq echo-keystrokes 0.3)


;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Easily NavigateSillyCased words when set to 0
(global-subword-mode 0)

;; Don't break lines for me, please
;;  But use mhs-trunc-lines to toggle bound to [C-c t]
(setq-default truncate-lines t)

;; A saner ediff
;; (setq ediff-diff-options "-w")
;; (setq ediff-split-window-function 'split-window-horizontally)
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; Bug in El Capitain with visible bell
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21662
;; This just fixes it sort of for a while.
(setq ring-bell-function
      #'(lambda () (message "*woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* ")(sleep-for .15)))


;; Extra Dired commands
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))


;; What command should be run from dired with 'dired-do-shell-command'
;; I don't know why the \\' in tif. regex
(setq dired-guess-shell-alist-user
      '(("\\.tif\\'" "display")
        ("\\.png\\'" "display")))


;; stolen from KWB-emacs
;;----------------------
;; show me the line numbers in source
(defun add-line-numbers ()
  (linum-mode 1))

;; auto wrap, but only in comments
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))


(defvar dev-hook nil
  "Hook that gets run on activation of any programming mode.")
(add-hook 'dev-hook 'add-line-numbers)
(add-hook 'dev-hook 'local-comment-auto-fill)
(add-hook 'dev-hook 'company-mode)

(defun run-dev-hook ()
  "Enable things that are convenient across all dev buffers."
  (run-hooks 'dev-hook))


;; show empty lines at the end of the file
(set-default 'indicate-empty-lines t)

;; automatically sync up external changes to files
;; (global-auto-revert-mode nil)
(defalias 'auto-revert-tail-mode 'tail-mode)

;; delete trailing whitespace
;; TODO [MHS, 2013-01-23] You should learn how to make this only for modes you want.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; To edit binary files
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 't))


;; Better file and buffer searching.
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))



;;----------------------------------------------------------------
;; Personal Lisp directory
;;----------------------------------------------------------------

;; Projectile is the BOMB!
(use-package projectile :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))



;; IDLWAVE Customizations
;; Load this before ruby because we want the jds history search.
(require 'emacs-idlwave-support)

;; Use jira information
(require 'mhs-jira)

;; Python environment
(require 'mhs-python)

(use-package markdown-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode) auto-mode-alist)))

;; Try to set up a ruby on rails environment.
;(require 'mhs-ruby-stuff)

(use-package paradox
  :ensure t
  :config
  (setq paradox-automatically-star t)
  (setq paradox-execute-asynchronously t))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; SICP

(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(add-hook 'scheme-mode-hook
          '(lambda ()
             (paredit-mode 1)))


;; Javascript stuff
(require 'mhs-javascript)

;; Load the BBDB if it's around.
(require 'mhs-bbdb)


;; if you are debugging emacs completely: open this file and it records keystrokes.
;;(open-dribble-file "~/dribble")

;; For orgmode and others start emacsclient
(server-start)

;; Do binary diff in hexl-mode if files are binary format
(require 'binary-diff)


;;--------------------------------------------------------------------------
;; GIT has some quirks and normally, I don't want to do a regular diff when
;; looking at revisions.

(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'ediff-revision))


;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff cutomizations
;;;;;;;;;;;;;;;;;;;;;;;;
(setq ediff-ignore-similar-regions 't)


;; 2009-08-12: Actually I don't like this on by default... Just remember you
;; can customize this variable.

;;  (ediff-diff-options "--ignore-matching-lines=^.*\\$.*\\$.*$")

;; Normally you want to use cmp to compare bunches of directories, but
;; sometimes, you really want to use diff with this option, If that's the
;; case, you need to customize group ediff-diff, and change the cmp command (to diff)
;; and ediff-cmp-options. (to --ignore-matching-lines=^.*\\$.*\\$.*$"

;; When you do this, you'll only see things that are actually different except
;; for things between dollar signs. like keywords.


(setq-default ediff-regexp-hide-A "^.*\$.*\$.*$")
(setq-default ediff-regexp-hide-B "^.*\$.*\$.*$")
(setq-default ediff-regexp-hide-C "^.*\$.*\$.*$")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read personal abbreviations each time
(quietly-read-abbrev-file (locate-user-emacs-file ".abbrev_defs"))
(setq abbrev-mode t)



;; Set the bookmark file in the right location.
(setq bookmark-default-file (locate-user-emacs-file ".emacs.bmk"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text mode (hook also run by related modes, e.g. Tex/LaTeX & outline)
(setq-default fill-column 77) ; Change it locally with [C-x f]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Makefile mode stuff
;;  you can comment this out if you don't like the "features" of this mode by
;;  placing a semicolin in front of the line.
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Emacs-Lisp stuff
;;
(add-to-list 'auto-mode-alist '("\\.emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("_emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.gnus" . emacs-lisp-mode))

;; Sh mode for some common files
(add-to-list 'auto-mode-alist '("\\.alias$" . sh-mode))


(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up some print commands
(setq lpr-command "/usr/bin/lpr -PHP4700 -h")


;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ------------------------------------------------------------
;; auto-insert madness
;; ------------------------------------------------------------
(auto-insert-mode 1)
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory (concat (file-name-as-directory user-emacs-directory) "autoinsert/"))
(add-to-list 'auto-insert-alist '("\\.pro$" . "idlwave-insert.pro"))




(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require the gnus stuff that I wrote...
(require 'mhs-map)
(require 'mhs-magit)
(require 'mhs-extends)
(require 'mhs-perl)
(require 'mhs-grep)
(require 'mhs-sii)
(require 'mhs-masie)
(require 'mhs-reindent)


(require 'mhs-cmode)
(require 'mhs-comment)


;; Handle multiple locations for aspell.
(defvar ispell-program-name)
(cond ((file-exists-p "/usr/bin/aspell")
       (setq ispell-program-name "/usr/bin/aspell"))
      ((file-exists-p "/usr/local/bin/aspell")
      (setq ispell-program-name "/usr/local/bin/aspell"))
      ((file-exists-p "/opt/local/bin/aspell")
      (setq ispell-program-name "/opt/local/bin/aspell"))
      (t (setq ispell-program-name "~savoie/local/bin/aspell")))



;; Use these to override stupid defaults for the ! command in dired.
(defvar dired-guess-shell-alist-user)
(setq dired-guess-shell-alist-user
      (list (list "\\.png\\'" "display")
            (list "\\.jpg\\'" "display")
            (list "\\.ps\\'" "gv")
            (list "\\.eps\\'" "gv")
            (list "\\.doc\\'" "ooffice")
            (list "\\.xls\\'" "ooffice")
            (list "\\.odg\\'" "ooffice")
            (list "\\.ods\\'" "ooffice")
            (list "\\.pdf\\'" "acroread") ))

;; Set registers to things that I type zillions of times.
;; ------------------------------------------------------

(set-register
 (string-to-char "p")
 "ftp://sidads.colorado.edu/pub/incoming/savoie")


;; colorize compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Stuff out of my custom
(setq save-abbrevs 't)
(setq scroll-bar-mode 'right)
(setq select-active-regions 't)

(provide 'emacs-extras)
