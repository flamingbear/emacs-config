;; File for extra emacs commands that are pretty standard, but useful
;;--------------------------------------------------------------------


;; Override having to type Yes or No with just Y or N
(fset 'yes-or-no-p 'y-or-n-p)
(set-scroll-bar-mode 'right)
(put 'scroll-left 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; To determine when to split horizontally
(setq split-width-threshold '1600)

(ansi-color-for-comint-mode-on)




;;----------------------------------------------------------------
;; General Lisp directory
;;----------------------------------------------------------------
(defvar mhs-lisp-dir (expand-file-name (concat emacs-top '"lisp"))
  "Directory for possible private *.el & *.elc files for customization when you
cannot put them directly in /usr/local/share/emacs/site-lisp

I generally choose ($EMACS_HOME)/lisp for my custom files:
/homes/nsidc-snowblower/savoie/.emacs.d/lisp" )
(if (file-accessible-directory-p mhs-lisp-dir)
    (add-to-list 'load-path mhs-lisp-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IDLWAVE Customizations
;; Load this before ruby because we want the jds history search.
(try-require 'emacs-idlwave-support)



;; 2011-08-09: <mhs> i've changed my python loading and now you have to have a
;; virtual environment set up before you start emacs if you want to use it
;; </mhs>
(setq mhs-pymacs-dir (getenv "VIRTUAL_ENV"))
(when mhs-pymacs-dir
    (setenv "PYMACS_DIR" mhs-pymacs-dir)
    (setenv "PYMACS_PYTHON" (concat (getenv "PYMACS_DIR") "/bin/python"))
    (require 'bcr-python))


;; Better file and buffer searching.
(require 'ido)

;; External lisp files
(defvar mhs-external-lisp-dir  (expand-file-name (concat emacs-top '"external-lisp-files/"))
  "Directory where lispy things I didn't write are put" )

;; Add all subdirectories of the external lisp dir to the load path.
(when (file-accessible-directory-p mhs-external-lisp-dir)
  (add-to-list 'load-path mhs-external-lisp-dir)
  (let ((default-directory  mhs-external-lisp-dir))
    (normal-top-level-add-subdirs-to-load-path)))



;; Try to set up a ruby on rails environment.
(try-require 'mhs-ruby-stuff)

(when (try-require 'yaml-mode)
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;; if external-lisp-dir has a slime directory, we will set up that
(try-require 'mhs-clisp-stuff)

;; Perl completion if possible
(when (try-require 'anything)
  (try-require 'perl-completion))

(try-require 'mhs-smarttabs)


;; Load the BBDB if it's around.
(try-require 'mhs-bbdb)

;; Try to use eclim for eclipse integration
;; 2011-10-02: <mhs>  Need to get Java working first.</mhs>
;;(try-require 'mhs-eclim)


;; Add a local site-lisp site.  2011-09-07: I'm not exactly sure why this is.
;; /home/savoie/local/share/emacs/site-lisp
(defvar mhs-local-site-lisp  (expand-file-name "~savoie/local/share/emacs/site-lisp"))
(when (file-accessible-directory-p mhs-local-site-lisp)
  (add-to-list 'load-path mhs-local-site-lisp))



;; if you are debugging emacs completely: open this file and it records keystrokes.
;;(open-dribble-file "~/dribble")

;; For orgmode and others start emacsclient
(server-start)


;; requires
;;----------
(try-require 'force-space)

;; Do binary diff in hexl-mode if files are binary format
(try-require 'binary-diff)

;; NCL mode
(when (try-require 'ncl)
  (add-to-list 'auto-mode-alist '("\\.ncl$" . ncl-mode)))



;; Extra Dired commands
(add-hook 'dired-load-hook
          (function (lambda ()
                      (load "dired-x")
                      ;; Set global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      )))

;; Cyclical Marking
;------------------
(when (try-require 'thing-cmds)
  (global-set-key [(control meta ? )] 'mark-thing) ; vs `mark-sexp'
  (global-set-key [(meta ?@)] 'cycle-thing-region)) ; vs `mark-word'



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PSVN for subversion integration
(unless (try-require 'psvn)
  (warn "Could't load psvn.el there is no subversion support. You may want to look at this. "))



;;--------------------------------------------------------------------------
;; GIT has some quirks and normally, I don't want to do a regular diff when
;; looking at revisions.

(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'ediff-revision))

(defvar mhs-git-helpers (expand-file-name (concat mhs-external-lisp-dir '"magit")))
(when (file-accessible-directory-p mhs-git-helpers)
  (add-to-list 'load-path mhs-git-helpers)
  (try-require 'magit))

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



;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

;; show time in mode line
(display-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up some print commands
(setq lpr-command "/usr/bin/lpr -Psnowprint -h")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set up html-helper-mode and font for it.
;; need the files html-helper-mode.el and tempo.el in your load-path
;; 2011-09-18: <mhs>  Taking this out, but the .el is in maybe-not-used</mhs>
;; (require 'html-helper-mode)
;; (add-to-list 'auto-mode-alist '("\\.asp" . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.html" . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.htm" . html-helper-mode))
;(add-to-list 'auto-mode-alist '("\\.xml" . html-helper-mode))

;; unique buffer names
(try-require 'uniquify)

;; ------------------------------------------------------------
;; I want template expansion!  use the auto-insert-tkld package
;; ------------------------------------------------------------
(when (try-require 'auto-insert-tkld)
  (setq auto-insert-alist (cons '("\\.cpp$" . "C++") auto-insert-alist))
  (add-to-list 'auto-insert-alist '("\\.pro$" . "idlwave"))
  (add-to-list 'auto-insert-type-alist '("idlwave" . "idlwave-insert.pro"))

  (add-to-list 'auto-insert-alist '("\\.R$" . "ess-file"))
  (add-to-list 'auto-insert-type-alist '("ess-file" . "ess-file.R"))

  (add-to-list 'auto-insert-alist '("\\.py$" . "python-file"))
  (add-to-list 'auto-insert-type-alist '("python-file" . "python-insert.py")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This allows me to use these normally disabled functions with out having to
;; tell emacs, "Yes, I really want to do this"


;; Allows you to separate out the parens and see them for what they are, and
;; jump from one end to the other with M-p
(autoload 't-match-move-point-to-matching "t-match" nil t nil)
(define-key global-map "\M-p" 't-match-move-point-to-matching)



(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require the gnus stuff that I wrote...
(define-prefix-command 'mhs-map)
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

;; preprocessor movement.
(try-require 'if-jump)


;; Handle multiple locations for aspell.
(cond ((file-exists-p "/usr/bin/aspell")
       (setq ispell-program-name "/usr/bin/aspell"))
      ((file-exists-p "/usr/local/bin/aspell")
      (setq ispell-program-name "/usr/local/bin/aspell"))
      (t (setq ispell-program-name "~savoie/local/bin/aspell")))



;; Use these to override stupid defaults for the ! command in dired.
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
;;             ;; possibly more rules ...
;;               (list "\\.bar\'";; rule with condition test
;;                     '(if condition
;;                           "BAR-COMMAND-1"
;;                         "BAR-COMMAND-2"))))

;; Set registers to things that I type zillions of times.
;; ------------------------------------------------------

(set-register
 (string-to-char "w")
 "while 1 do wdelete")

(set-register
 (string-to-char "u")
 ";;----------------------------------------------------------------------\n")

(set-register
 (string-to-char "f")
 "fprintf(stdout, \"\\n\");")

(set-register
 (string-to-char "p")
 "ftp://sidads.colorado.edu/pub/incoming/savoie")
