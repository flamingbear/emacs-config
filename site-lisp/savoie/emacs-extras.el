;;; package --- Summary
;;;   Just standard extra things to load when I start emacs.


;;; Code:
(use-package gnus
  :defer 3
  :init
  (setq gnus-init-file (locate-user-emacs-file ".gnus"))
  (load-file gnus-init-file)
  :config
  (setq gnus-select-method (quote (nntp "nntp.aioe.org")))
  (setq gnus-verbose 10)
  (setq gnus-treat-hide-signature t)
  (setq gnus-treat-display-x-face 'head)
  (setq gnus-thread-sort-functions '(gnus-thread-sort-by-number))
  (setq gnus-agent-expire-days '90)
  (setq smtpmail-smtp-server "smtp.colorado.edu")
  (setq smtpmail-smtp-service "587")
  (setq smtpmail-smtp-user "savoie")
  (setq smtpmail-stream-type (quote starttls))
  (setq send-mail-function 'smtpmail-send-it)
  (setq nnmail-crosspost nil)
  (setq nnmail-expiry-wait 3)
  (setq starttls-extra-arguments '("--no-ca-verification"))
  (setq message-cite-function 'message-cite-original-without-signature)
  (setq message-mode-hook '(turn-on-flyspell turn-off-auto-fill turn-on-visual-line-mode))
  (setq message-send-mail-partially-limit nil)
  (setq mm-verify-option 'always)
  (setq mail-source-delete-incoming t)
  (setq mail-user-agent 'message-user-agent)
  (setq gnus-suppress-duplicates t)
  (setq gnus-summary-ignore-duplicates t)
  )


;; Override having to type Yes or No with just Y or N
(fset 'yes-or-no-p 'y-or-n-p)
(put 'scroll-left 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; To determine when to split horizontally
(setq split-width-threshold '1600)
(setq gc-cons-threshold 20000000)


(setq indent-tabs-mode nil)
(setq inhibit-eol-conversion t)
(setq inhibit-startup-screen t)


;; Some magnar's Sane Defaults.
;;------------------------------
;; Show keystrokes in progress
(setq echo-keystrokes 0.3)


;; Remove text in active region if inserting text in non-nil
(setq delete-selection-mode 'nil)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Easily NavigateSillyCased words when set to 0
(global-subword-mode 0)

;; Don't break lines for me, please
;;  But use mhs-trunc-lines to toggle bound to [C-c t]
(setq-default truncate-lines t)

;; Toggle selective display based on current column
;; https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
;; Use C-u C-u to set selective display to current line's indentation.
(defun set-selective-display-current (arg)
  "Toggle fold all lines larger than indentation on current line"
  (interactive "P")
  (if (eq (first arg) 16)
      (let ((col 1))
	(save-excursion
	  (back-to-indentation)
	  (setq col (+ 1 (current-column)))
	  (set-selective-display
	   (if selective-display nil (or col 1)))))
    (set-selective-display arg)))
(global-set-key (kbd "C-x $") 'set-selective-display-current)

(when (file-exists-p (locate-file "highlight-indentation.el" load-path))
  (load-file (locate-file "highlight-indentation.el" load-path))
  (highlight-indentation-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode))

(use-package smart-shift
  :ensure t
  :config
  (global-smart-shift-mode 1))


(setq tab-always-indent 'complete)
(tool-bar-mode -1)
(setq transient-mark-mode 't)

;; Bug in El Capitain with visible bell and emacs 24.x
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21662
(defun my-zoidberg-bell ()
  "Ring visible bell with text in minibuffer."
  (message "*woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop*")
  (sleep-for .15))

(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell 't)
(if running-macos (setq ring-bell-function #'my-terminal-visible-bell) (setq ring-bell-function #'my-zoidberg-bell))


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
(setq show-trailing-whitespace t)

;; automatically sync up external changes to files
;;(global-auto-revert-mode nil)
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
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



;;----------------------------------------------------------------
;; Personal Lisp directory
;;----------------------------------------------------------------

;; Projectile is the BOMB!
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'ivy)

  ;; HACK until this is fixed: https://github.com/bbatsov/projectile/issues/1165
  (defun projectile-discover-projects-in-directory (directory)
    "Discover any projects in DIRECTORY and add them to the projectile cache.
  This function is not recursive and only adds projects with roots
  at the top level of DIRECTORY."
    (interactive
     (list (read-directory-name "Starting directory: ")))
    (let ((subdirs (directory-files directory t)))
      (mapcar
       (lambda (dir)
         (when (and (file-directory-p dir)
                    (not (member (file-name-nondirectory dir) '(".." "."))))
           (let ((default-directory dir)
                 (projectile-cached-project-root dir))
             (when (projectile-project-p)
               (projectile-add-known-project (projectile-project-root))))))
       subdirs)))

    ;; "Fix" for stupidly slow emacs https://github.com/bbatsov/projectile/issues/1183
    (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name)))))

(use-package grep
  :defer 3
  :config
  (setq grep-find-command
        "find . -name \".svn\" -prune -o -type f  -exec grep -nH \"\" {} \\;"))

(use-package wgrep :ensure t)

(use-package puppet-mode :ensure t)
(use-package gist :ensure t)


;; IDLWAVE Customizations
;; Load this before ruby because we want the jds history search.
(use-package emacs-idlwave-support)

;; Use jira information
(use-package mhs-jira)

;; Python environment
(use-package mhs-python)

;; Now we're a docker house.
(use-package dockerfile-mode
  :ensure t)

(use-package docker
  :ensure t
  :diminish docker-mode
  :config
  (docker-global-mode))

(use-package docker-compose-mode
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package markdown-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode) auto-mode-alist))
  (when running-macos
    (setq markdown-command "Markdown.pl"))
  )


(use-package paradox
  :defer t
  :ensure t
  :config
  (setq paradox-automatically-star t)
  (setq paradox-execute-asynchronously t)
  (load (expand-file-name "private/paradox-secrets.el.gpg" user-emacs-directory))
  )

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;; Try company-mode instead of auto-complete
;;--------------------------
(use-package company-jedi :ensure t)
(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-to-list 'company-backends 'company-jedi)
  )


(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package paren
  :defer 3
  :config
  (show-paren-mode t))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; SICP
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(add-hook 'scheme-mode-hook
          '(lambda ()
             (paredit-mode 1)))


;; Javascript stuff
(use-package mhs-javascript)

;; Load the BBDB if it's around.
(use-package mhs-bbdb)


;; if you are debugging emacs completely: open this file and it records keystrokes.
;;(open-dribble-file "~/dribble")

;; For orgmode and others start emacsclient
(server-start)

;; Do binary diff in hexl-mode if files are binary format
(use-package binary-diff)


;;--------------------------------------------------------------------------
;; GIT has some quirks and normally, I don't want to do a regular diff when
;; looking at revisions.

(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'ediff-revision))


;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff cutomizations
;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ediff-ignore-similar-regions)
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
;; 2017-04-25:  I think this is OBE.
;; (setq-default ediff-regexp-hide-A "^.*\$.*\$.*$")
;; (setq-default ediff-regexp-hide-B "^.*\$.*\$.*$")
;; (setq-default ediff-regexp-hide-C "^.*\$.*\$.*$")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read personal abbreviations each time
(quietly-read-abbrev-file (locate-user-emacs-file ".abbrev_defs"))
(setq abbrev-mode t)



;; Set the bookmark file in the right location.
(setq bookmark-default-file (locate-user-emacs-file ".emacs.bmk"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text mode (hook also run by related modes, e.g. Tex/LaTeX & outline)
(setq-default fill-column 79) ; Change it locally with [C-x f]


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


;; try windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; ------------------------------------------------------------
;; auto-insert madness
;; ------------------------------------------------------------
(use-package autoinsert
  :config
  (setq auto-insert-path '("~savoie/.emacs.d/autoinsert/insert/"))
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert-directory (concat (file-name-as-directory user-emacs-directory) "autoinsert/"))
  (add-to-list 'auto-insert-alist '("\\.pro$" . "idlwave-insert.pro")))

(use-package jka-compr
  :defer 3
  :config
  (setq auto-compression-mode t))

(use-package bookmark
  :config
  (setq bookmark-save-flag 1))


(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require the gnus stuff that I wrote...
(use-package mhs-map)
(use-package mhs-magit)
(use-package mhs-extends)
(use-package mhs-perl)
(use-package mhs-grep)
(use-package mhs-sii)
(use-package mhs-reindent)


(use-package mhs-cmode)
(use-package mhs-comment)


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

;; Dired extra commands
(use-package dired
  :config
  (setq dired-listing-switches "-al"))

(use-package dired-x
  :config
  (setq dired-guess-shell-alist-user
	(list
	 '("\\.gif$" "display")
	 '("\\.pdf" "open")
	 )))

(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))


(use-package shell-pop
  :ensure
  :bind (([f3] . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  (setq shell-pop-window-size 30)
  (setq shell-pop-full-span t)
  (setq shell-pop-window-position "bottom")
  ;; may need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  )

(use-package pinentry
  :ensure t
  :config
  (pinentry-start)
  (setq epa-pinentry-mode 'loopback)
   ;;(setq epg-gpg-program "/usr/local/bin/gpg")
  )

;; possible packages     (pinentry anything auto-complete)

(use-package helpful
  ;; Helpful is an alternative to the built-in Emacs help that provides much
  ;; more contextual information.
  :ensure t
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; I also recommend you configure helpful-at-point to a convenient keybinding:
  (global-set-key (kbd "C-c C-.") #'helpful-at-point) )


;; Set registers to things that I type zillions of times.
;; ------------------------------------------------------

(set-register
 (string-to-char "p")
 "ftp://sidads.colorado.edu/pub/incoming/savoie")

;; run display on last "created" image
(fset 'mhs-display-last-created-image
      "\C-rcreat\C-m\C-[f\C-f\C-@\C-e\C-[w\C-[>display \C-y&")



;; Stuff out of my custom
(setq save-abbrevs 't)
(setq scroll-bar-mode 'right)
(setq select-active-regions 't)

(setq large-file-warning-threshold nil)



(provide 'emacs-extras)
;;; emacs-extras.el ends here
