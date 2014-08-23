;; File for extra emacs commands that are pretty standard, but useful
;;--------------------------------------------------------------------


;; Override having to type Yes or No with just Y or N
(fset 'yes-or-no-p 'y-or-n-p)
(set-scroll-bar-mode 'nil)
(put 'scroll-left 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; To determine when to split horizontally
(setq split-width-threshold '1600)

(ansi-color-for-comint-mode-on)


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
(add-hook 'def-hook 'auto-complete-mode)

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

(global-flycheck-mode 't)


;; Better file and buffer searching.
(require 'ido)




;;----------------------------------------------------------------
;; Personal Lisp directory
;;----------------------------------------------------------------
(defvar mhs-lisp-dir (expand-file-name (concat (file-name-as-directory emacs-top) "lisp"))
  "Directory for possible private *.el & *.elc files for customization

I choose for my custom files:
$EMACS_TOP/lisp" )
(if (file-accessible-directory-p mhs-lisp-dir)
    (add-to-list 'load-path mhs-lisp-dir))



;; External lisp files
(defvar mhs-external-lisp-dir  (expand-file-name (concat (file-name-as-directory emacs-top) "external-lisp-files/"))
  "Directory where lispy things I didn't write are put" )

;; Add all subdirectories of the external lisp dir to the load path.
(when (file-accessible-directory-p mhs-external-lisp-dir)
  (add-to-list 'load-path mhs-external-lisp-dir)
  (let ((default-directory  mhs-external-lisp-dir))
    (normal-top-level-add-subdirs-to-load-path)))


;; IDLWAVE Customizations
;; Load this before ruby because we want the jds history search.
(try-require 'emacs-idlwave-support)

;; set pivotal-tracker api tolken (defunct)
(try-require 'mhs-pivotal)

;; Use jira information
(try-require 'mhs-jira)

;; Python environment
(try-require 'mhs-python)


;; Set up Magnars' subdirs.
(defvar magnars-stuff (concat mhs-external-lisp-dir "magnars/"))
(when (file-accessible-directory-p magnars-stuff)
  (add-to-list 'load-path magnars-stuff))


(when (try-require 'markdown-mode)
  (setq auto-mode-alist
         (cons '("\\.md" . markdown-mode) auto-mode-alist)))

;; Try to set up a ruby on rails environment.
(try-require 'mhs-ruby-stuff)


(when (try-require 'yaml-mode)
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;; Javascript stuff
(try-require 'mhs-javascript)

;; If there's a clojure setup, use it.
(try-require 'mhs-clojure)

;; if external-lisp-dir has a slime directory, we will set up that
(try-require 'mhs-clisp-stuff)

;; Perl completion if possible
(when (try-require 'anything)
  (try-require 'perl-completion))


;; Load the BBDB if it's around.
(try-require 'mhs-bbdb)


;; if you are debugging emacs completely: open this file and it records keystrokes.
;;(open-dribble-file "~/dribble")

;; For orgmode and others start emacsclient
(server-start)

;; Do binary diff in hexl-mode if files are binary format
(try-require 'binary-diff)

;; jump-to and set workspaces.
(try-require 'mhs-workspace)


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
(quietly-read-abbrev-file (concat emacs-top ".abbrev_defs"))

;; Set the bookmark file in the right location.
(setq bookmark-default-file (expand-file-name ".emacs.bmk" emacs-top))

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

;; ------------------------------------------------------------
;; auto-insert madness
;; ------------------------------------------------------------
(auto-insert-mode 1)
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory (concat (file-name-as-directory emacs-top) "autoinsert/"))
(add-to-list 'auto-insert-alist '("\\.cpp$" . "c++-insert.cc"))
(add-to-list 'auto-insert-alist '("\\.pro$" . "idlwave-insert.pro"))
(add-to-list 'auto-insert-alist '("\\.py$" . "python-insert.pl"))


;; Allows you to separate out the parens and see them for what they are, and
;; jump from one end to the other with M-p
(autoload 't-match-move-point-to-matching "t-match" nil t nil)
(define-key global-map "\M-p" 't-match-move-point-to-matching)



(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require the gnus stuff that I wrote...
(try-require 'mhs-map)
(try-require 'mhs-extends)
(try-require 'mhs-perl)
(try-require 'mhs-grep)
(try-require 'mhs-sii)
(try-require 'mhs-cdr)
(try-require 'mhs-masie)
(try-require 'mhs-reindent)


(try-require 'mhs-cmode)
(try-require 'mhs-comment)


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
